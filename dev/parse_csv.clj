(ns parse-csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn fix-csv [from to]
  (let [lines (str/split-lines (slurp from))
        sb    (StringBuilder.)]
    (loop [lines  lines
           delims 0]
      (when (seq lines)
        (let [line        (first lines)
              line-delims (count (re-seq #";" line))
              delims'     (+ delims line-delims)]
          (when (pos? delims)
            (.append sb "\\n"))
          (.append sb line)
          (when (> delims' 172)
            (prn delims' line))
          (if (>= delims' 172)
            (do
              (.append sb "\n")
              (recur (next lines) 0))
            (recur (next lines) delims')))))
    (spit "notebooks/vehicles.csv" (str sb))))

(defn read-csv [path]
  (with-open [reader (io/reader path)]
    (doall
     (csv/read-csv reader :separator \; :quote \'))))

(defn parse-value [s]
  (cond
    (str/starts-with? s ":")
    (keyword (subs s 1))

    (re-matches #"[1-9][0-9]*" s)
    (parse-long s)

    (re-matches #"\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d\d\d)?Z" s)
    (java.util.Date/from (java.time.Instant/parse s))

    (= "" s)
    nil

    :else
    s))

(defn keywordize [s]
  (-> s
      (str/replace " " "-")
      str/lower-case
      keyword))

(defn save [data path]
  (let [columns [;; Status
                 ["Expected entry datetime" "Expected"]
                 ["Entry datetime" "Entry"]
                 ["Exit expected at" "Expected exit"]
                 ["Exit datetime" "Exit"]
                 "Delivery dealer code"
                 ["Expected customer delivery date" "Expected customer delivery"]
                 "Blocked"
               ;; ?? Damaged :fahrzeug/damages
                 "Customs number"
                 "Workflow"
                 ["Parking spot" "Parked at"]

               ;; Vehicle
                 "VIN"
                 "Make"
                 "Model"
                 "Engine type"
                 "Customer"
                 "Weight"
               ;; ?? Service

               ;; Transport
                 ["Entry transport order carrier code" ["Entry transport order" "carrier code"]]
                 ["Entry transport order mode" ["Entry transport order" "mode"]]
                 ["Entry transport order origin location" ["Entry transport order" "origin location"]]
                 ["Entry transport order spediteur" ["Entry transport order" "spediteur"]]]
        indices (mapv #(.indexOf (first data) (if (vector? %) (first %) %)) columns)
        columns' (->> columns
                      (map #(if (vector? %) (second %) %))
                      (mapv #(if (string? %) [(keywordize %)] (mapv keywordize %))))
        f       (fn [row]
                  (reduce
                   (fn [m [idx col]]
                     (assoc-in m col (parse-value (nth row idx))))
                   {}
                   (map vector indices columns')))
        data'   (->> data (drop 1) #_(take 100) (map f))]
    (taoensso.nippy/freeze-to-file path data')
    data'))

(comment
  (fix-csv "/Users/tonsky/Downloads/Download.csv" "/Users/tonsky/Downloads/vehicles.csv")
  (def data
    (read-csv "/Users/tonsky/Downloads/vehicles.csv"))
  (save data "notebooks/vehicles.nippy"))
