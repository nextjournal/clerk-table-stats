(ns ^:nextjournal.clerk/no-cache table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as table-stats]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]))

(comment
  (clerk/serve! {})
  (clerk/halt!)
  (clerk/clear-cache!))

123
;; - [x] Port grouping/nesting feature from ductile
;; - [ ] Port some [stats](https://github.com/nextjournal/clerk/pull/156/files) stuff from Philippa in here?
;;    - [ ] compute-table-summary
;; - [ ] Think about how to query the remote data source for more information
;; - [x] honey.sql issue with clerk show!

(def query-results
  (let [_run-at #inst "2021-05-20T08:28:29.445-00:00"
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "chinook.db"})]
    (with-open [conn (jdbc/get-connection ds)]
      (clerk/table (jdbc/execute! conn (sql/format {:select [:AlbumId :Bytes :Name :TrackID
                                                             :UnitPrice]
                                                    :from :tracks}))))))
