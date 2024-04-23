(ns ^:nextjournal.clerk/no-cache table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as table-stats]
            ))

(comment
  (clerk/serve! {}))

(def x (range 100))
