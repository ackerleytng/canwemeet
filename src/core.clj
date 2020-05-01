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
    (if (t/after? time maybe-next)
      (t/plus maybe-next (t/days 1))
      maybe-next)))

(def ranges
  {:morning [5 10]
   :working [10 17]
   :evening [17 23]})

(defn preferences
  "Hour-of-day for calls, in order of preference:

  early in the evening [17 - 21)
  as late as possible in the morning hours [8 - 10)
  night [21 - 24)
  as late as possible early morning [5 - 8)
  lunchtime (12)
  lunchtime (13)
  earliest overlapping hour after lunch (14 - 17)
  latest overlapping hour before lunch (10 - 12)"
  []
  (concat
   (range 17 21)
   (reverse (range 8 10))
   (range 21 24)
   (reverse (range 5 8))
   [12 13]
   (range 14 17)
   (reverse (range 10 12))))

(defn within-range
  "Whether t is l <= t, t + hours < h (assumes that l is before h)"
  [[l h] hours t]
  (and (not (t/after? l t))
       (t/before? (t/minus
                   (t/plus t (t/hours hours))
                   (t/minutes 1))
                  h)))

(defn recommend-time-within-range
  "Given a time range in a time zone, recommend a meeting time in the same time
  zone. Will return a recommended time according to (preferences)."
  [range]
  (first
   (filter (partial within-range range 1)
           (map (partial forward-to-next (first range)) (preferences)))))

(def range-wordings
  {:morning "Their mornings are"
   :working "They are at work"
   :evening "Their evenings are"})

(def wording-times
  (map #(map % [range-wordings ranges]) (keys range-wordings)))

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

(defn recommend-meeting-time
  "Given user time and target time now, recommend a meeting time in the user time
  zone, for a meeting of duration hours. hours defaults to 1"
  ([user-time-now their-time-now]
   (recommend-meeting-time user-time-now their-time-now 1))
  ([user-time-now their-time-now hours]
   (let [their-working-range-user-tz (map #(t/with-zone-same-instant
                                             (forward-to-next their-time-now %) user-time-now)
                                          (:working ranges))
         recommended-time-user-tz (recommend-time-within-range their-working-range-user-tz)]
     recommended-time-user-tz)))

(defn format-recommended-meeting-time
  ([user-time-now their-time-now]
   (format-recommended-meeting-time user-time-now their-time-now 1))
  ([user-time-now their-time-now hours]
   (let [recommended-time-format "h a, EE d MMMM"
         recommended-time-user-tz (recommend-meeting-time user-time-now their-time-now hours)
         recommended-time-their-tz (when recommended-time-user-tz
                                     (t/with-zone-same-instant
                                       recommended-time-user-tz
                                       their-time-now))]
     (if recommended-time-user-tz
       [(str "Tell them you want to meet at "
             (t/format recommended-time-format recommended-time-their-tz)
             ", their time")
        (str "That's "
             (t/format recommended-time-format recommended-time-user-tz)
             ", your time")]
       ["Can't find a good time to meet!"]))))

(defn format-output-lines
  [user-time-now {:keys [formatted time-zone dst-observed time-zone-id]}]
  (let [their-time-now (t/with-zone-same-instant user-time-now time-zone-id)
        recs (format-recommended-meeting-time user-time-now their-time-now)
        meta (str (s/join " " [formatted "is in the" time-zone "time zone"])
                  (if dst-observed " (DST is observed now)"))
        times (map (partial format-range user-time-now their-time-now) wording-times)]
    (concat
     recs
     [meta]
     times)))

(defn meet-info [user-time-now address]
  (let [location (geocode address)
        data (merge location (time-zone (:latlng location)))]
    (format-output-lines user-time-now data)))

(defn usage []
  ["Usage: canwemeet ADDRESS-OR-LOCATION"
   "Example:"
   "  canwemeet austin"
   ""
   "I'm using the following definitions:"
   "  + Mornings: 5 am to 10 am"
   "  + Working hours: 10 am to 5 pm"
   "  + Evenings: 5 pm to 11 pm"])

(defn -main [& args]
  (println
   (s/join
    "\n"
    (if (not (< 0 (count args) 2))
      (usage)
      (meet-info (user-time) (first args))))))

(comment
  (-main ["san francisco"])
  )
