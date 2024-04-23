(ns ^:nextjournal.clerk/no-cache table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as table-stats]
            [next.jdbc :as jdbc]))

(comment
  (clerk/serve! {}))

(def x (range 100))

;; ## SQL Queries
(def query-results
  (let [_run-at #inst "2021-05-20T08:28:29.445-00:00"
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "chinook.db"})]
    (with-open [conn (jdbc/get-connection ds)]
      (clerk/table (jdbc/execute! conn ["SELECT AlbumId, Bytes, Name, TrackID, UnitPrice FROM tracks"])))))
