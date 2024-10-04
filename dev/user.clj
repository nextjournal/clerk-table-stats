(ns user
  (:require [nextjournal.clerk :as clerk]))

(future
  (Thread/sleep 1000)
  (clerk/show! "notebooks/table_filters.clj")
  (clerk/serve! {:port 7890 :render-nrepl {} :browse true}))
