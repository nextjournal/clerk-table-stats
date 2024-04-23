;; # 📊 Clerk Table Stats
^{:nextjournal.clerk/visibility :hide-ns}
(ns nextjournal.clerk-table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as viewer]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn deep-merge [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (last xs)))]
    (reduce m maps)))

(defn paths->head [paths]
  (->> paths
       (group-by first)
       (map (fn [[k paths]] (if (< 1 (count paths)) [k (mapv (comp first rest) paths)] k)))))

(defn head->paths [head]
  (mapcat (fn [k]
            (if (vector? k)
              (mapv #(vector (first k) %) (last k))
              [[k]])) head))

(defn ->ordered-paths [order paths]
  (->> paths
       (sort-by #(let [i (.indexOf order %)]
                   (if (< i 0) (count paths) i)))
       vec))

(defn ->visible-paths [hidden paths]
  (reduce (fn [ps p]
            (if (contains? hidden p) ps (vec (conj ps p))))
          []
          paths))

(defn normalize-seq-of-map
  ([s] (normalize-seq-of-map {} s))
  ([{:keys [column-order computed-columns hide-columns group-headers schema]} s]
   (let [paths (if schema
                 (head->paths schema)
                 (->> s
                      (apply deep-merge)
                      (mapcat (fn [[k v]]
                                (if (and (map? v) group-headers)
                                  (map #(if (or (true? group-headers) (contains? (set group-headers) k))
                                          (vector k %)
                                          [k])
                                       (keys v))
                                  [[k]])))
                      (remove nil?)
                      distinct))
         computed-paths (set (map vector (keys computed-columns)))
         ordered-paths (cond->> paths
                         (and (not schema) (seq computed-columns)) (concat computed-paths)
                         (seq column-order) (->ordered-paths (head->paths column-order)))
         hidden-paths (if (seq hide-columns) (set (head->paths hide-columns)) #{})
         visible-paths (cond->> ordered-paths
                         (seq hidden-paths) (->visible-paths hidden-paths))]
     {:schema (paths->head ordered-paths)
      :paths ordered-paths
      :hidden-paths hidden-paths
      :head (paths->head visible-paths)
      :rows (map (fn [m] (map (fn [path]
                                (if (contains? computed-paths path)
                                  ((get-in computed-columns path) m)
                                  (get-in m path viewer/missing-pred))) visible-paths)) s)})))

(defn normalize-map-of-seq
  ([s] (normalize-map-of-seq {} s))
  ([{:keys [column-order hide-columns group-headers schema]} m]
   (let [paths (if schema
                 (head->paths schema)
                 (->> m
                      (mapcat (fn [[k s]]
                                (if (and group-headers (map? (first s)))
                                  (let [mm (apply deep-merge s)]
                                    (map #(if (or (true? group-headers) (contains? (set group-headers) k))
                                            (vector k %)
                                            [k])
                                         (keys mm)))
                                  [[k]])))
                      distinct))
         ordered-paths (cond->> paths
                         (seq column-order) (->ordered-paths (head->paths column-order)))
         hidden-paths (if (seq hide-columns) (set (head->paths hide-columns)) #{})
         visible-paths (cond->> ordered-paths
                         (seq hidden-paths) (->visible-paths hidden-paths))]
     {:schema (paths->head ordered-paths)
      :paths ordered-paths
      :hidden-paths hidden-paths
      :head (paths->head visible-paths)
      :rows (map-indexed (fn [i _] (map (fn [[k1 k2]]
                                          (let [v (nth (get m k1) i viewer/missing-pred)]
                                            (if k2
                                              (get v k2)
                                              v)))
                                        visible-paths))
                         (val (apply max-key (comp viewer/count-bounded val) m)))})))

(defn normalize-table-data
  ([data] (normalize-table-data {} data))
  ([opts data]
   (cond
     (and (map? data) (-> data (viewer/get-safe :rows) sequential?)) (viewer/normalize-seq-to-vec data)
     (and (map? data) (sequential? (first (vals data)))) (normalize-map-of-seq opts data)
     (and (sequential? data) (map? (first data))) (normalize-seq-of-map opts data)
     (and (sequential? data) (sequential? (first data))) (viewer/normalize-seq-of-seq data)
     :else nil)))

(def table-markup-viewer
  {:render-fn '(fn [head+body opts]
                 [:div.bg-white.rounded-lg.border.border-slate-300.shadow-sm.font-sans.text-sm.not-prose.overflow-x-auto
                  {:class "print:overflow-none print:text-[10px] print:shadow-none print:rounded-none print:border-none"}
                  ;; (prn (:render-fn (:nextjournal/viewer (first head+body))))
                  (into
                   [:table.w-full]
                   (nextjournal.clerk.render/inspect-children opts)
                   head+body)])})

#_(defn header-cell [{:keys [sub-headers? nested? header-cell index opts]}]
    (let [v (cond
              nested? (last header-cell)
              (vector? header-cell) (first header-cell)
              :else header-cell)
          k (if (and (not nested?) (vector? header-cell)) (first header-cell) header-cell)
          title (when (or (string? v) (keyword? v) (symbol? v)) v)
          {:keys [translated-keys column-layout number-col? filters update-filters! !expanded-at] :or {translated-keys {}}} opts]
      [:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.border-slate-300.text-center.whitespace-nowrap.border-b
       (cond-> {:class (str
                        "print:text-[10px] print:bg-transparent print:px-[5px] print:py-[2px] "
                        (when (and sub-headers? nested?) "first:border-l-0 ")
                        (if (and (ifn? number-col?) (number-col? index)) "text-right " "text-left "))}
         (and column-layout (column-layout k)) (assoc :style (column-layout k))
         (and (not nested?) (vector? header-cell)) (assoc :col-span (count (first (rest header-cell))))
         (and sub-headers? (not (vector? header-cell))) (assoc :row-span 2)
         title (assoc :title title))
       [:div (get translated-keys v (trk v))]
       (when-let [[component choices-or-label :as filter] (get filters k)]
         (case component
           :checkbox (let [selected (get-in @!expanded-at [:filters k] false)]
                       [:label.w-full.flex.items-center.gap-1.justify-center {:style {:height 30}}
                        [:input {:type :checkbox
                                 :on-change (fn []
                                              (swap! !expanded-at update-in [:filters v] not)
                                              (update-filters! k filter (not selected)))
                                 :checked (or selected false)}] choices-or-label])
           :multi-select (let [selected (get-in @!expanded-at [:filters k] #{})]
                           [multi-select {:choices choices-or-label
                                          :selected-value selected
                                          :show-selected? true
                                          :placeholder "Filter…"
                                          :on-change (fn [val]
                                                       (let [new-selected (conj selected val)]
                                                         (swap! !expanded-at assoc-in [:filters k] new-selected)
                                                         (update-filters! k filter new-selected)))
                                          :on-remove (fn [val]
                                                       (let [new-selected (disj selected val)]
                                                         (swap! !expanded-at assoc-in [:filters k] new-selected)
                                                         (update-filters! k filter new-selected)))}])))]))


(def table-head-viewer
  {:render-fn '
   (fn [header-row {:as opts :keys [path]}]
     (let [header-cells (nextjournal.clerk.viewer/desc->values header-row)
           sub-headers (remove nil? (mapcat #(when (vector? %) (second %)) header-cells))
           sub-headers? (seq sub-headers)]
       [:thead
        (into [:tr.print:border-b-2.print:border-black
               ]
              (map-indexed (fn [idx cell]
                             (let [header-cell cell]
                               ((fn [{:keys [sub-headers? nested? header-cell index opts]}]
                                  (let [v (cond
                                            nested? (last header-cell)
                                            (vector? header-cell) (first header-cell)
                                            :else header-cell)
                                        k (if (and (not nested?) (vector? header-cell)) (first header-cell) header-cell)
                                        title (when (or (string? v) (keyword? v) (symbol? v)) v)
                                        {:keys [translated-keys column-layout number-col? filters update-filters! !expanded-at] :or {translated-keys {}}} opts]
                                    [:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.border-slate-300.text-center.whitespace-nowrap.border-b
                                     (cond-> {:class (str
                                                      "print:text-[10px] print:bg-transparent print:px-[5px] print:py-[2px] "
                                                      (when (and sub-headers? nested?) "first:border-l-0 ")
                                                      (if (and (ifn? number-col?) (number-col? index)) "text-right " "text-left "))}
                                       (and column-layout (column-layout k)) (assoc :style (column-layout k))
                                       (and (not nested?) (vector? header-cell)) (assoc :col-span (count (first (rest header-cell))))
                                       (and sub-headers? (not (vector? header-cell))) (assoc :row-span 2)
                                       title (assoc :title title))
                                     [:div v]
                                     #_(when-let [[component choices-or-label :as filter] (get filters k)]
                                       (case component
                                         :checkbox (let [selected (get-in @!expanded-at [:filters k] false)]
                                                     [:label.w-full.flex.items-center.gap-1.justify-center {:style {:height 30}}
                                                      [:input {:type :checkbox
                                                               :on-change (fn []
                                                                            (swap! !expanded-at update-in [:filters v] not)
                                                                            (update-filters! k filter (not selected)))
                                                               :checked (or selected false)}] choices-or-label])
                                         :multi-select (let [selected (get-in @!expanded-at [:filters k] #{})]
                                                         [multi-select {:choices choices-or-label
                                                                        :selected-value selected
                                                                        :show-selected? true
                                                                        :placeholder "Filter…"
                                                                        :on-change (fn [val]
                                                                                     (let [new-selected (conj selected val)]
                                                                                       (swap! !expanded-at assoc-in [:filters k] new-selected)
                                                                                       (update-filters! k filter new-selected)))
                                                                        :on-remove (fn [val]
                                                                                     (let [new-selected (disj selected val)]
                                                                                       (swap! !expanded-at assoc-in [:filters k] new-selected)
                                                                                       (update-filters! k filter new-selected)))}])))]))
                                {:sub-headers? sub-headers? :header-cell cell :opts opts :index idx})


                               #_[:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.border-slate-300.text-center.whitespace-nowrap.border-b
                                  (cond-> {:class (str
                                                   "print:text-[10px] print:bg-transparent print:px-[5px] print:py-[2px] "
                                                   (when (and sub-headers? false #_nested?) "first:border-l-0 ")
                                                   #_(if (and (ifn? number-col?) (number-col? index)) "text-right " "text-left "))}
                                    (and sub-headers? (not (vector? header-cell))) (assoc :row-span 2))
                                  cell #_(nextjournal.clerk.render/inspect-presented opts cell)]) #_(ductile.clerk.render/header-cell {:sub-headers? (seq sub-headers)
                                                                                                                                       :header-cell cell
                                                                                                                                       :opts opts
                                                                                                                                       :index idx})))
              header-cells)
        #_(prn :sub-headers sub-headers)
        (when-not (empty? sub-headers)
          (into [:tr.print:border-b-2.print:border-black]
                (map-indexed (fn [idx cell]
                               [:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.border-slate-300.text-center.whitespace-nowrap.border-b
                                cell #_(nextjournal.clerk.render/inspect-presented opts cell)]

                               )#_(ductile.clerk.render/header-cell {:header-cell %2
                                                                     :opts opts
                                                                     :index %1}))
                sub-headers))]))})

(def table-body-viewer
  {:render-fn '(fn [rows opts] (into [:tbody] (map-indexed (fn [idx row] (nextjournal.clerk.render/inspect-presented (update opts :path conj idx) row))) rows))})

(def table-row-viewer
  {:render-fn '(fn [row {:as opts :keys [path number-col?]}]
                 (into [:tr.print:border-b-gray-500.hover:bg-gray-200.print:hover:bg-transparent
                        {:class (str "print:border-b-[1px] "
                                     (if (even? (peek path)) "bg-white" "bg-slate-50"))}]
                       (map-indexed
                        (fn [idx cell]
                          [:td.px-4.py-2.text-sm.border-r.last:border-r-0
                           {:class (str "print:text-[10px] print:bg-transparent print:px-[5px] print:py-[2px] "
                                        (when (and (ifn? number-col?) (number-col? idx)) "text-right"))}
                           (nextjournal.clerk.render/inspect-presented opts cell)]))
                       row))})

(def ductile-table-viewer
  (assoc viewer/table-viewer
         :transform-fn
         (fn [{:as wrapped-value :nextjournal/keys [applied-viewer render-opts]}]
           (if-let [{:keys [head rows]} (normalize-table-data render-opts (viewer/->value wrapped-value))]
             (-> wrapped-value
                 (assoc :nextjournal/viewer table-markup-viewer)
                 (update :nextjournal/width #(or % :wide))
                 (update :nextjournal/render-opts merge {:num-cols (count (or head (first rows)))
                                                         :number-col? (into #{}
                                                                            (comp (map-indexed vector)
                                                                                  (keep #(when (number? (second %)) (first %))))
                                                                            (not-empty (first rows)))})
                 (assoc :nextjournal/value (cond->> []
                                             (seq rows) (cons (viewer/with-viewer table-body-viewer (merge (-> applied-viewer
                                                                                                               (select-keys [:page-size])
                                                                                                               (set/rename-keys {:page-size :nextjournal/page-size}))
                                                                                                           (select-keys wrapped-value [:nextjournal/page-size]))
                                                                (map (partial viewer/with-viewer table-row-viewer) rows)))
                                             head (cons (viewer/with-viewer (:name table-head-viewer table-head-viewer) head)))))
             (-> wrapped-value
                 viewer/mark-presented
                 (assoc :nextjournal/width :wide)
                 (assoc :nextjournal/value [(viewer/present wrapped-value)])
                 (assoc :nextjournal/viewer {:render-fn 'nextjournal.clerk.render/render-table-error}))))))

(viewer/reset-viewers! :default
                       (viewer/add-viewers (viewer/get-default-viewers)
                                           [ductile-table-viewer]))

{::clerk/visibility {:code :hide :result :show}}

(comment
  ;; ## Seq of map

  (def seq-of-map
    [{:ars/id "1"
      :compound/name "Krefeld"
      :ductile/id #uuid "1174774f-17ec-442c-803f-2906015be68f"
      :entry/datetime #inst "2023-09-28T06:33:01Z"}
     {:ars/id "2"
      :compound/name "Krefeld"
      :ductile/id #uuid "774f1174-7ec1-2c44-3f80-15be68f29060"}])

  (clerk/example
    (normalize-seq-of-map seq-of-map)
    (normalize-seq-of-map {:column-order [:compound/name :entry/datetime]
                           :hide-columns [:ars/id :ductile/id]} seq-of-map)
    (normalize-seq-of-map {:column-order [:compound/abbreviation :compound/name :entry/datetime]
                           :hide-columns [:ars/id :ductile/id]
                           :computed-columns {:compound/abbreviation (fn [m]
                                                                       (str/upper-case (str/join (take 3 (:compound/name m)))))}} seq-of-map))

  (clerk/table seq-of-map)
  (clerk/table {::clerk/render-opts {:column-order [:compound/name :entry/datetime]}} seq-of-map)
  (clerk/table {::clerk/render-opts {:hide-columns [:ars/id :ductile/id]}} seq-of-map)
  #_(clerk/table {::clerk/render-opts {:column-order [#_:compound/abbreviation :compound/name :entry/datetime]
                                       :hide-columns [:ars/id :ductile/id]
                                       :computed-columns {:compound/abbreviation (fn [m]
                                                                                   (str/upper-case (str/join (take 3 (:compound/name m)))))}}} seq-of-map))

;; ## Nested seq of map

(def nested-seq-of-map
  [{:ars/id "1"
    :compound/name "Krefeld"
    :ductile/id #uuid "1174774f-17ec-442c-803f-2906015be68f"
    :entry/datetime #inst "2023-09-28T06:33:01Z"
    :entry/transport {:transport/mode :mode/truck
                      :transport/name "Kempers"}
    :exit/transport {:transport/mode :mode/truck
                     :transport/name "Kempers"}}
   {:ars/id "2"
    :compound/name "Krefeld"
    :ductile/id #uuid "774f1174-7ec1-2c44-3f80-15be68f29060"
    :entry/transport {:transport/mode :mode/truck
                      :transport/name "Kempers"}
    :exit/transport {:transport/mode :mode/truck}}])

(clerk/example
  #_(normalize-seq-of-map nested-seq-of-map)
  (normalize-seq-of-map {:group-headers true} nested-seq-of-map)
  (normalize-seq-of-map {:group-headers [:entry/transport]} nested-seq-of-map)
  (normalize-seq-of-map {:group-headers true
                         :column-order [:compound/name
                                        [:entry/transport [:transport/name :transport/mode]]
                                        [:exit/transport [:transport/name :transport/mode]]
                                        :entry/datetime]
                         :hide-columns [:ars/id :ductile/id]} nested-seq-of-map))

(clerk/table {::clerk/render-opts {:group-headers true}} nested-seq-of-map)
(clerk/table {::clerk/render-opts {:group-headers [:entry/transport]}} nested-seq-of-map)
(clerk/table {::clerk/render-opts {:group-headers true
                                   :column-order [:compound/name
                                                  [:entry/transport [:transport/name :transport/mode]]
                                                  [:exit/transport [:transport/name :transport/mode]]
                                                  :entry/datetime]
                                   :hide-columns [:ars/id :ductile/id]}} nested-seq-of-map)
