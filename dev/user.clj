(ns user
  (:require [nextjournal.clerk :as clerk]))

(clerk/serve! {:port 7890 :render-nrepl {}})
