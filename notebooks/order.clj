(ns ^:nextjournal.clerk/no-cache order
  (:require [nextjournal.clerk :as clerk]
            ;; loaded for side effects
            [nextjournal.clerk-table-stats :as clerk-table-stats]
            ))
(comment
  (clerk/serve! {})
  (clerk/halt!)
  (clerk/clear-cache!)
  (remove-ns 'scratch))

(clerk/with-viewer clerk-table-stats/viewer
  {::clerk/render-opts {:stats false
                        :column-order [:a :b :c]}}
  [{:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9}])

(comment
  (clerk-table-stats/normalize-seq-of-map [{:pred inc} {:pred {:wrapped :ns?}}])
  )
