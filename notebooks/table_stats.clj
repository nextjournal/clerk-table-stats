(ns ^:nextjournal.clerk/no-cache table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as clerk-table-stats] ;; loaded for side effects
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [tablecloth.api :as tc]))

(comment
  (clerk/serve! {})
  (clerk/halt!)
  (clerk/clear-cache!))

;; ## Research

;; - [x] Check out Observable data table cell (SQL example: https://observablehq.com/@observablehq/working-with-sql)
;; - [ ] Protocol/API to provide your own `:stats` rather than letting the table viewer calculate them
;; - [ ] API to provide your own `:schema` such that clerk doesn't have to normalize all the data
;; - [ ] check out how this would work with table cloth large columnar data sets
;;   - [ ] skip mapcat stuff
;;   - [ ] take 1000 like with clerk tableviewer
;;   - [ ] should work with infinite seqs in map-of-seq

;; - [ ] also with datomic qseq in ductile

;; ## Concrete stuff to work on
;; - [ ] Make filters work client side (check Obserable for example)
;;   - [x] By clicking on diagram
;;   - [x] How to preserve state over multiple `:render-fn`, perhaps via `:render-opts`? No, by `inspect-children`
;;   - [ ] By clicking on filter, new values should be fetched, according to filter
;;   - [ ] Or by selecting values from column (see Observable Data Table Cell)
;;   - [ ] Scrubbing
;;   - [ ] export filter code to be used as clerk option

#_ (reset! !table-state {:filter {}})

#_
(def my-data
  [{:category :foo :value 10}
   {:category :bar :value 20}
   {:category :foo :value 10}
   {:category :bar :value 15}
   {:category :bar :value 22}
   {:category :baz :value 12}])

(clerk/with-viewer clerk-table-stats/viewer
  [{:category :bang :value 11}
   {:category :barx :value 20}
   {:category :bang :value 10}
   {:category :barx :value 15}
   {:category :barx :value 22}
   {:category :bug :value 12}
   {:category :bug :value 22}])

#_ @table-stats/anon-expr-5dstzZ7ATg61zHQuMY4MFbrkNN8qmE-table

#_ (clerk/recompute!)

(clerk/with-viewer clerk-table-stats/viewer
  [{:category :bang :value 1}
   {:category :barx :value 2}
   {:category :bang :value 1}
   {:category :barx :value 1}
   {:category :barx :value 2}
   {:category :bug :value 1}
   {:category :bug :value 2}])

(def query-results
  (let [_run-at #inst "2021-05-20T08:28:29.445-00:00"
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "chinook.db"})]
    (with-open [conn (jdbc/get-connection ds)]
      (clerk/with-viewer clerk-table-stats/viewer
        (jdbc/execute! conn (sql/format {:select [:albums.title :Bytes :tracks.Name :TrackID
                                                  :UnitPrice :artists.Name]
                                         :from :tracks
                                         :join [:albums [:= :tracks.AlbumId :albums.AlbumId]
                                                :artists [:= :artists.ArtistId :albums.ArtistId]]}))))))

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
    :compound/name "Lonato"
    :ductile/id #uuid "774f1174-7ec1-2c44-3f80-15be68f29060"
    :entry/transport {:transport/mode :mode/truck
                      :transport/name "Kempers"}
    :exit/transport {:transport/mode :mode/truck}}])

(clerk/with-viewer clerk-table-stats/viewer
  {::clerk/render-opts {:group-headers true
                        :column-order [:compound/name
                                       [:entry/transport [:transport/name :transport/mode]]
                                       [:exit/transport [:transport/name :transport/mode]]
                                       :entry/datetime]
                        :hide-columns [:ars/id :ductile/id]}}
  nested-seq-of-map)


;; classic map of seq, columnar data
(def DS (tc/dataset {:V1 (take 9 (cycle [1 2]))
                     :V2 (range 1 10)
                     :V3 (take 9 (cycle [0.5 1.0 1.5]))
                     :V4 (take 9 (cycle ["A" "B" "C"]))
                     :V5 (range)}))



(tc/dataset [{:a 1} {:a 1 :b 2}])

(comment

  (reset! table-stats/query-results-table {:filter {}})

  @anon-expr-5drwytFZT7UnMsYvtXSdRi6gU5KCTr-table
  (reset! nextjournal.clerk.webserver/!doc nil)
  )
