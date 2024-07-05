(ns nested-tabular-data
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as clerk-table-stats] ;; loaded for side effects
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [nextjournal.clerk.viewer :as viewer]
            [tablecloth.api :as tc]))

(defn tabular? [xs]
  (and (seqable? xs)
       (sequential? xs)
       (let [sample (take 100 xs)]
         (and (every? map? sample)
              (some? (clerk-table-stats/normalize-seq-of-map {:group-headers true} sample))))))

(def view-as-table-viewer
  {:pred {:wrapped (every-pred (comp #{1} count :path)
                               (comp tabular? viewer/->value))}
   :transform-fn (partial clerk/with-viewer 'nextjournal.clerk.viewer/table-viewer
                          {::clerk/render-opts {:group-headers true}
                           ::clerk/page-size 5
                           ::clerk/width :full})})

(clerk/add-viewers! [view-as-table-viewer
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
