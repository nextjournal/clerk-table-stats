;; # Table Filters

(ns ^:nextjournal.clerk/no-cache table-filters
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as clerk-table-stats]))

(clerk/with-viewer clerk-table-stats/viewer
  {::clerk/render-opts {:filters {:category :text}
                        :stats true}}
  [{:category :bang :value 11}
   {:category :barx :value 20}
   {:category :bang :value 10}
   {:category :barx :value 15}
   {:category :barx :value 22}
   {:category :bug :value 12}
   {:category :bug :value 22}])
