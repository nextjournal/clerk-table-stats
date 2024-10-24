;; # Table Filters
(ns table-filters
  {:nextjournal.clerk/no-cache true
   :nextjournal.clerk/visibility {:code :hide
                                  :result :hide}}
  (:require [clojure.math :as math]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as clerk-table-stats]))

(def cities
  {"United Kingdom" ["London" "Westminster" "Birmingham" "Leeds" "Glasgow" "Manchester" "Sheffield" "Bradford" "Edinburgh" "Liverpool"]
   "Germany"        ["Berlin" "Hamburg" "Munich" "Cologne" "Frankfurt am Main" "Stuttgart" "Düsseldorf" "Leipzig" "Dortmund" "Essen"]
   "USA"            ["New York" "Los Angeles" "Chicago" "Houston" "Phoenix" "Philadelphia" "San Antonio" "San Diego" "Dallas" "Jacksonville"]
   "Россия"         ["Москва" "Санкт-Петербург" "Новосибирск" "Екатеринбург" "Казань" "Нижний Новгород" "Челябинск" "Красноярск" "Самара" "Уфа"]
   "Australia"      ["Sydney" "Melbourne" "Brisbane" "Perth" "Adelaide" "Hobart" "Darwin"]
   "Republic of the Congo" ["Brazzaville" "Pointe-Noire" "Dolisie" "Nkayi" "Impfondo" "Ouésso" "Madingou" "Owando" "Sibiti" "Loutété"]})

(def last-names
  ["Smith" "Johnson" "Williams" "Brown" "Jones" "Miller" "Davis" "Garcia" "Rodriguez" "Wilson"
   "Martinez" "Anderson" "Taylor" "Thomas" "Hernandez" "Moore" "Martin" "Jackson" "Thompson" "White" "Lopez" "Lee" "Gonzalez"])

(def first-names
  ["Liam" "Noah" "Oliver" "James" "Elijah" "Mateo" "Theodore" "Henry" "Lucas" "William"
   "John" "Robert" "Michael" "David" "Richard" "Charles" "Joseph" "Thomas"
   "Olivia" "Emma" "Charlotte" "Amelia" "Sophia" "Mia" "Isabella" "Ava" "Evelyn" "Luna"
   "Mary" "Patricia" "Linda" "Barbara" "Elizabeth" "Jennifer" "Maria" "Susan" "Margaret" "Dorothy"])

(defn rand-el [coll]
  (cond
    (< (rand) 0.05)
    nil

    (empty? coll)
    nil

    :else
    (-> (rand) (* math/PI) math/sin (* (dec (count coll))) math/round (->> (nth coll)))))

(defn some-map [& kvs]
  (into {} (keep (fn [[k v]] (when (some? v) [k v]))) (partition 2 kvs)))

(defn row-fn [id]
  (let [country (rand-el (keys cities))]
    (some-map
     :id       id
     :location (some-map
                :country country
                :city    (rand-el (cities country)))
     :age      (rand-int 100)
     :height   (+ 150 (rand-int 50))
     :married? (rand-nth [true false nil])
     :gender   (rand-nth [:male :female :unknown nil])
     :name     (some-map
                :first (rand-el first-names)
                :last  (rand-el last-names)))))

(defonce data
  (mapv row-fn (range 10000)))

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/with-viewer clerk-table-stats/viewer
  {::clerk/render-opts {:filters {[:location :country] :multiselect
                                  [:location :city]    :text
                                  :age                 :regexp
                                  :height              :text
                                  :married?            :multiselect
                                  :gender              :multiselect
                                  [:name :first]       :multiselect
                                  [:name :last]        :text}
                        :group-headers true
                        :hide-columns [:id]
                        :stats false}}
  data)

; - [ ] Sorting
; - [ ] Search with placeholder (glob patterns)
; - [ ] Not operator
; - [ ] Rename project to clerk-data-tables
; - [ ] Sort numbers as numbers
; - [ ] Better data structure to store active filters
; - [ ] checkbox filter
; - [ ] arrow keys in multiselect
; - [ ] Make ranges/histogram pretty
; - [ ] :unique doesn’t work
; - [ ] Resize should go into react tree
; - [ ] Resize should be in %
; - [ ] Resize on first load of incomplete data
; - [ ] Maybe move stats behind a button
; - [ ] Column reorder
; - [ ] Hide columns
; - [ ] Subheaders in clerk
; - [ ] Unicode-aware filtering
; - [x] Maybe move filters behind a button
; - [x] type inside multiselect
; - [x] regexp filter
; - [x] abstract away filter fn
; - [x] rename :filter -> :active-filters
; - [x] share filter state between stats and filters
; - [x] Populate pre-set filters on first load
; - [x] reset in :text filter
; - [x] Portal for popup
; - [x] Scroll dropdown content if it doesn’t fit on screen
; - [x] do not close popup after first selection
; - [x] use checkboxes instead of checkmarks
