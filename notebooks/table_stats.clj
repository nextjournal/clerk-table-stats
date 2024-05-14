(ns ^:nextjournal.clerk/no-cache table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats] ;; loaded for side effects
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [tablecloth.api :as tc]))

(comment
  (clerk/serve! {})
  (clerk/halt!)
  (clerk/clear-cache!))

;; ## Research
;; - [ ] Check out Observable data call (SQL example)
;; - [ ] Protocol/API to provide your own `:stats` rather than letting the table viewer calculate them
;; - [ ] API to provide your own `:schema,` such that clerk doesn't have to normalize all the data
;; - [ ] check out how this would work with table cloth large columnar data sets
;;   - [ ] don't know how skipping normalization buys us anything since you need to transpose the data anyway when sending it to the client to render it as a table, so what problem are we solving again?
;; - [ ] also with datomic qseq in ductile

;; ## Concrete stuff to work on
;; - [ ] Make filters work client side (check Obserable for example)
;;   - [ ] By clicking on diagram
;;   - [ ] Or by selecting values from column
;;   - [ ] Scrubbing

(def my-data
  [{:category :foo :value 10}
   {:category :bar :value 20}
   {:category :foo :value 10}
   {:category :bar :value 15}
   {:category :bar :value 22}
   {:value 12}])

(clerk/table my-data)


(def query-results
  (let [_run-at #inst "2021-05-20T08:28:29.445-00:00"
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "chinook.db"})]
    (with-open [conn (jdbc/get-connection ds)]
      (clerk/table (jdbc/execute! conn (sql/format {:select [:albums.title :Bytes :Name :TrackID
                                                             :UnitPrice]
                                                    :from :tracks
                                                    :join [:albums [:= :tracks.AlbumId :albums.AlbumId]]}))))))

(def row-count
  (jdbc/execute! {:dbtype "sqlite" :dbname "chinook.db"}
                 (sql/format {:select [[[:count :*]]]
                              :from :tracks})))

(def nested-seq-of-map
  [{:ars/id "1"
    :compound/name "Krefeld"
    :ductile/id #uuid "1174774f-17ec-442c-803f-2906015be68f"
    :entry/datetime #inst "2023-09-28T06:33:01Z"
    :entry/transport {:transport/mode :mode/truck
                      :transport/name "Kempers"}
    :exit/transport {:transport/mode :mode/truck
                     :transport/name "Kempers"}}
   {:ars/id "2"
    :compound/name "Krefeld"
    :ductile/id #uuid "774f1174-7ec1-2c44-3f80-15be68f29060"
    :entry/transport {:transport/mode :mode/truck
                      :transport/name "Kempers"}
    :exit/transport {:transport/mode :mode/truck}}])

(clerk/table {::clerk/render-opts {:group-headers true
                                   :column-order [:compound/name
                                                  [:entry/transport [:transport/name :transport/mode]]
                                                  [:exit/transport [:transport/name :transport/mode]]
                                                  :entry/datetime]
                                   :hide-columns [:ars/id :ductile/id]}} nested-seq-of-map)


;; classic map of seq, columnar data
(def DS (tc/dataset {:V1 (take 9 (cycle [1 2]))
                     :V2 (range 1 10)
                     :V3 (take 9 (cycle [0.5 1.0 1.5]))
                     :V4 (take 9 (cycle ["A" "B" "C"]))
                     :V5 (range)}))



(tc/dataset [{:a 1} {:a 1 :b 2}])
