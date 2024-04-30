(ns ^:nextjournal.clerk/no-cache table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats] ;; loaded for side effects
            [next.jdbc :as jdbc]
            [honey.sql :as sql]))

(comment
  (clerk/serve! {})
  (clerk/halt!)
  (clerk/clear-cache!))

;; - [x] Port grouping/nesting feature from ductile
;; - [x] Port some [stats](https://github.com/nextjournal/clerk/pull/156/files) stuff from Philippa in here?
;;    - [x] compute-table-summary
;; - [ ] Think about how to query the remote data source for more information
;; - [x] honey.sql issue with clerk show!

(def query-results
  (let [_run-at #inst "2021-05-20T08:28:29.445-00:00"
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "chinook.db"})]
    (with-open [conn (jdbc/get-connection ds)]
      (clerk/table (jdbc/execute! conn (sql/format {:select [:albums.title :Bytes :Name :TrackID
                                                             :UnitPrice]
                                                    :from :tracks
                                                    :join [:albums [:= :tracks.AlbumId :albums.AlbumId]]}))))))

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

