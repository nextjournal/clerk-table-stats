(ns nested-tabular-data
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as clerk-table-stats]))

#_ (clerk/add-viewers! [])
(clerk/add-viewers! [clerk-table-stats/view-as-table-viewer
                     clerk-table-stats/viewer])

{:nextjournal.clerk/visibility {:code :hide :result :show}}

[{:category :bang
  :value 11}
 {:category :barx
  :value 20}
 {:category :bug
  :value 22
  :children [{:category :bang :value 11}
             {:category :barx :value 20}]}]
