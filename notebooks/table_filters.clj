;; # Table Filters

(ns ^:nextjournal.clerk/no-cache table-filters
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as clerk-table-stats]))

(clerk/with-viewer clerk-table-stats/viewer
  {::clerk/render-opts {:filters {[:location :country] :multiselect
                                  [:location :city] :text
                                  :value :text}
                        :group-headers true
                        :hide-columns [:id]
                        :stats true}}
  [{:id 0 :location {:country :uk :city "London"} :value 11}
   {:id 1 :location {:country :uk :city "Brighton"} :value 12}
   {:id 2 :location {:country :uk :city "London"} :value 13}
   {:id 3 :location {:country :germany :city "Berlin"} :value 21}
   {:id 4 :location {:country :germany :city "Münich"} :value 22}
   {:id 5 :location {:country :germany :city "Berlin"} :value 22}
   {:id 6 :location {:country :usa :city "New York"} :value 11}
   {:id 7 :location {:country :usa :city "San Francisco"} :value 12}
   {:id 8 :location {:country :usa :city "San Francisco"} :value 22}])
