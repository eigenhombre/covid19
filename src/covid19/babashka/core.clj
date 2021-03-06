(ns covid19.babashka.core
  (:require [babashka.curl :as curl]
            [clojure.data.csv :as csv]
            [clojure.string :as string])
  (:import [java.time LocalDate]))

(def csv-url (str "https://raw.githubusercontent.com/"
                  "CSSEGISandData/COVID-19/master/"
                  "csse_covid_19_data/csse_covid_19_time_series/"
                  "time_series_covid19_confirmed_US.csv"))

;; Don't fetch this over and over again when reloading file in REPL:
(defonce rawdata (->> csv-url
                      curl/get
                      :body))

(defn normalize-records
  [raw]
  (let [[headers & rows] (csv/read-csv raw)]
    (for [r rows]
      (merge {:days (->> r
                         (drop 11)
                         (map #(Double. %)))}
             (zipmap (take 11 (map keyword headers))
                     (take 11 r))))))

(defn selected-rows [& filters]
  (reduce (fn [acc f]
            (filter f acc))
          (normalize-records rawdata)
          filters))

(defn selected-cases [{:keys [region state locale]}]
  (selected-rows (comp (partial = region) :Country_Region)
                 (comp (partial = state) :Province_State)
                 (comp (partial = locale) :Admin2)))

(defn table-str [rows]
  (let [ncols (count (first rows))
        col-lengths (for [c (range ncols)]
                      (->> rows
                           (map #(nth % c 0))
                           (map str)
                           (map count)
                           (apply max)))]
    (string/join
     "\n"
     (for [r rows]
       (string/join " "
                    (for [c (range ncols)]
                      (format (format "%%%ds" (nth col-lengths c))
                              (nth r c "?"))))))))

(defn -main [& [region state locale]]
  (if-not (and region state locale)
    (println "Usage: covid19 <region-or-country> <province-or-state> <county-or-locale>")
    (let [daily-cases
          (->> (selected-cases {:region region, :state state, :locale locale})
               first
               :days
               (map int))
          weeks (partition-all 7 daily-cases)
          weekly-deltas (map (comp (partial apply -)
                                   (juxt last first))
                             weeks)]
      (println "Data starts 2020-01-22; today's date:" (str (LocalDate/now)))
      (println "Weeks:")
      (println (table-str (concat [["W" "Th" "F" "Sa" "Su" "M" "Tu"]
                                   ["--" "--" "--" "--" "--" "--" "--"]]
                                  weeks)))
      (println "\nSeven day average of daily increase:\n")
      (println (table-str (concat [["W" "Th" "F" "Sa" "Su" "M" "Tu"]
                                   ["--" "--" "--" "--" "--" "--" "--"]]
                                  (->> daily-cases
                                       (partition 2 1)
                                       (map (comp (partial apply -) reverse))
                                       (partition 7 1)
                                       (map (comp int #(/ % 7) (partial apply +)))
                                       (partition-all 7)))))
      (println "\nWeekly new:\n")
      (println (->> weekly-deltas
                    (partition-all 4)
                    table-str)))))
