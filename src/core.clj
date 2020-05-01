(ns core
  (:gen-class)
  (:require [clj-http.client :as client]
            [java-time :as t]
            [clojure.string :as s]))

(defn api-key []
  (or (System/getenv "APIKEY")
      (throw (Exception. "Please set APIKEY as environment variable"))))

(defn geocode [address]
  (let [extract (juxt :formatted_address (comp :location :geometry))
        params {"key" (api-key)
                "address" address}
        resp (client/get
              "https://maps.googleapis.com/maps/api/geocode/json"
              {:as :json
               :query-params params})]
    (zipmap [:formatted :latlng]
            (->> resp
                 :body
                 :results
                 first
                 extract))))

(defn- format-latlng [latlng]
  (str (:lat latlng) "," (:lng latlng)))

(defn time-zone [latlng]
  (let [params {"location" (format-latlng latlng)
                ;; Using machine's timezone since all we want to get is the
                ;;   time zone information, will rely on java's time zone for
                ;;   DST adjustments
                "timestamp" (.toEpochSecond (t/zoned-date-time))
                "key" (api-key)}
        resp (client/get "https://maps.googleapis.com/maps/api/timezone/json"
                         {:as :json
                          :query-params params})
        body (:body resp)]
    {:time-zone-id (:timeZoneId body)
     :time-zone (:timeZoneName body)
     :dst-observed (not= (:dstOffset body) 0)}))

(defn user-time
  "Will eventually find a way to use get user's time in user's timezone, maybe from IP
  geolocation, but use system time for now"
  []
  (t/zoned-date-time))

(defn forward-to-next
  "Given time, advance it to the next time it is the given hour

  For example, if time is Monday 1300h, and hour is 5 (0500), return Tuesday 0500.
  If time is Monday 1300h, and hour is 18 (1800), return Monday 1800."
  [time hour]
  (let [maybe-next (t/adjust time (t/local-time hour))]
    (if (< maybe-next time)
      (t/plus maybe-next (t/days 1))
      maybe-next)))

(def ranges
  {"Their mornings are" [5 10]
   "They are in the office" [10 17]
   "Their evenings are" [17 23]})

(defn format-range [user-time-now their-time-now [range [l h]]]
  (let [low (t/format "h a" (t/with-zone-same-instant
                              (forward-to-next their-time-now l)
                              user-time-now))
        high (t/format "h a" (t/with-zone-same-instant
                               (forward-to-next their-time-now h)
                               user-time-now))]
    (s/join " " [range "between"
                 (s/lower-case low) "and"
                 (s/lower-case high) "your time"])))

(defn format-output-lines
  [user-time-now {:keys [formatted time-zone dst-observed time-zone-id]}]
  (let [their-time-now (t/with-zone-same-instant user-time-now time-zone-id)
        header (str (s/join " " [formatted "is in the" time-zone "time zone"])
                    (if dst-observed " (DST is observed now)"))
        times (map (partial format-range user-time-now their-time-now) ranges)]
    (concat [header] times)))

(defn meet-info [user-time-now address]
  (let [location (geocode address)
        data (merge location (time-zone (:latlng location)))]
    (format-output-lines user-time-now data)))

(defn usage []
  ["Usage: canwemeet ADDRESS-OR-LOCATION"
   "Example:"
   "  canwemeet austin"])

(defn -main [& args]
  (println
   (s/join
    "\n"
    (if (not (< 0 (count args) 2))
      (usage)
      (meet-info (user-time) (first args))))))

(-main ["austin"])
