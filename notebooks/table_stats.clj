(ns ^:nextjournal.clerk/no-cache table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as table-stats]
            [next.jdbc :as jdbc]))

(comment
  (clerk/serve! {}))

;; - [ ] Port some [stats](https://github.com/nextjournal/clerk/pull/156/files) stuff from Philippa in here?
;;    - [ ] compute-table-summary
;; - [ ] Think about how to query the remote data source for more information


;; ## SQL Queries
(def query-results
  (let [_run-at #inst "2021-05-20T08:28:29.445-00:00"
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "chinook.db"})]
    (with-open [conn (jdbc/get-connection ds)]
      (jdbc/execute! conn ["SELECT AlbumId, Bytes, Name, TrackID, UnitPrice FROM tracks"]))))

(clerk/table query-results)
