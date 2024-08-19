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
   {:id 5 :location {:country :germany} :value 22}
   {:id 6 :location {:city "New York"} :value 11}
   {:id 7 :location {:country :usa :city "San Francisco"} :value 12}
   {:id 8 :location {:country :australia :city "Melburn"} :value 22}
   {:id 8 :location {:country :republic-of-the-congo :city "Brazzaville"} :value 22}])

; - [ ] checkbox filter
; - [ ] regex filter
; - [ ] rename :filter key
; - [x] do not close popup after first selection
; - [x] use checkboxes instead of checkmarks
; - [x] share filter state between stats and filters
; - [ ] abstract away filter fn
; - [x] reset in :text filter
; - [ ] type inside multiselect
; - [ ] arrow keys in multiselect
; - [x] Portal for popup
; - [x] Scroll dropdown content if it doesn’t fit on screen
; - [ ] Make ranges/histogram pretty
; - [ ] :unique doesn’t work
; - [ ] Resize should go into react tree
; - [ ] Resize should be in %
; - [x] Populate pre-set filters on first load
; - [ ] Resize on first load of incomplete data
; - [ ] Maybe move filters behind a button
; - [ ] Maybe move stats behind a button
; - [ ] Column reorder
; - [ ] Hide columns