#!/usr/bin/env bb

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

(def t0 (LocalDate/parse "2020-01-22"))

(defn normalize-records
  [raw]
  (let [[headers & rows] (csv/read-csv raw)]
    (for [r rows]
      (do
        (merge {:days (->> r
                           (drop 11)
                           (map-indexed vector)
                           (map (fn [[i x]] {:n (Double. x)
                                             :i i
                                             :day (.plusDays t0 i)})))}
               (zipmap (take 11 (map keyword headers))
                       (take 11 r)))))))

(defn selected-rows [& filters]
  (reduce (fn [acc f]
            (filter f acc))
          (normalize-records rawdata)
          filters))

(defn cook-cases [{:keys [region state locale]}]
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
    (println (str "Usage: covid19 "
                  "<region-or-country> "
                  "<province-or-state> "
                  "<county-or-locale>"))
    (let [daily-cases
          (->> (cook-cases {:region region, :state state, :locale locale})
               first
               :days
               (map (juxt :day (comp int :n))))
          weeks (partition-all 7 daily-cases)]
      (println "Data starts 2020-01-22; today's date:" (str (LocalDate/now)))
      (println "Weeks:")
      (println (table-str (concat [["W" "Th" "F" "Sa" "Su" "M" "Tu"]
                                   ["--" "--" "--" "--" "--" "--" "--"]]
                                  (map (partial map second) weeks))))
      (println "\nSeven day average of daily increase:\n")
      (println (table-str (concat [["W" "Th" "F" "Sa" "Su" "M" "Tu"]
                                   ["--" "--" "--" "--" "--" "--" "--"]]
                                  (->> daily-cases
                                       (map second)
                                       (partition 2 1)
                                       (map (comp (partial apply -) reverse))
                                       (partition 7 1)
                                       (map (comp int
                                                  #(/ % 7)
                                                  (partial apply +)))
                                       (partition-all 7)))))
      (println "\nWeekly new:\n")
      (printf "%s %15s\n" "Week Starting" "Case Count")
      (let [out (str
                 (clojure.string/join
                  "\n"
                  (for [w (butlast weeks)]
                    (let [[[fd fn] [ld ln]] [(first w) (last w)]]
                      (format "%s %15d"
                              fd (- ln fn)))))
                 "\n")
            filename (format "/tmp/covid19-%s-%s-%s.dat"
                             region state locale)]
        (println out)
        (spit filename out)

        (spit "/tmp/plotit.gnu" (format "
set term png enhanced font 'Verdana,10'
set output '/tmp/covid19.png'
set xdata time
set timefmt \"%%Y-%%m-%%d\"
set format x \"%%Y-%%m-%%d\"
set xtics rotate
set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 2
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 2
plot '%s' using 1:2 with lines linestyle 1
" filename))
        (prn (clojure.java.shell/sh
              "gnuplot"
              :in (clojure.java.io/file "/tmp/plotit.gnu")))
        (clojure.java.shell/sh "open" "/tmp/covid19.png")))))

(ns user (:require [covid19.babashka.core]))
(apply covid19.babashka.core/-main *command-line-args*)
