;; # Table Search
(ns table-search
  {:nextjournal.clerk/budget nil
   :nextjournal.clerk/no-cache true
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as clerk-table-stats]))

(def data
  (taoensso.nippy/thaw-from-file "notebooks/vehicles.nippy"))

#_^{:nextjournal.clerk/visibility {:result :show}}
  (clerk/table data)

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/with-viewer clerk-table-stats/viewer
  {::clerk/render-opts {:group-headers true
                        :column-order [:expected
                                       :entry
                                       :expected-exit
                                       :exit
                                       :delivery-dealer-code
                                       :expected-customer-delivery
                                       :blocked
                                       :customs-number
                                       :workflow
                                       :parked-at
                                       :vin
                                       :make
                                       :model
                                       :engine-type
                                       :customer
                                       :weight
                                       [:entry-transport-order [:carrier-code :mode :origin-location :spediteur]]]}}
  data)

;; - [x] up/down arrows
;; - [x] clicking on result
;; - [x] inserting autocompleted result
;; - [ ] actual filtering:
;;   - [x] substring
;;   - [ ] glob
;;   - [x] +/-
;;   - [ ] dates
;; - [x] get more realistic data
;; - [ ] search for multiple values (e.g. make:bmw,ford,kia)
;; - [ ] tolerate some reasonable amount of whitespace
;; - [ ] glob patterns in autocomplete
;; - [ ] top-level search
;; - [ ] >/>=/</<= syntax
;; - [ ] dates autocomplete (calendar)
;; - [ ] exact match syntax
;; - [x] restore filter after reload