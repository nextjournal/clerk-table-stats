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
  (taoensso.nippy/thaw-from-file "notebooks/dispatchable_vehicles.nippy"))

#_^{:nextjournal.clerk/visibility {:result :show}}
  (first data)

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/with-viewer clerk-table-stats/viewer
  {::clerk/render-opts {:group-headers true
                        :column-order [:vehicle/vin
                                       :make/name
                                       :vehicle/model
                                       :vehicle/weight
                                       [:transport/origin [:address/city :address/street]]
                                       [:transport/destination [:address/city :address/street]]
                                       :fahrzeug/expected-customer-delivery-datetime]

                        :select-columns [:make/name
                                         :vehicle/model
                                         :vehicle/vin
                                         :vehicle/weight
                                         [:transport/origin [:address/city :address/street]]
                                         [:transport/destination [:address/city :address/street]]
                                         :fahrzeug/expected-customer-delivery-datetime]}}
  data)

;; - [x] up/down arrows
;; - [x] clicking on result
;; - [x] inserting autocompleted result
;; - [ ] actual filtering:
;;   - [x] substring
;;   - [ ] glob
;;   - [x] +/-
;;   - [ ] dates
;; - [ ] get more realistic data
;; - [ ] glob patterns in autocomplete
;; - [ ] top-level search
;; - [ ] >/>=/</<= syntax
;; - [ ] dates autocomplete (calendar)
;; - [ ] exact match syntax