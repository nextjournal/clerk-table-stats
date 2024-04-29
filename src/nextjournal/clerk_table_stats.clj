;; # 📊 Clerk Table Stats
^{:nextjournal.clerk/visibility :hide-ns}
(ns nextjournal.clerk-table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as viewer]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def table-head-viewer-fn
  '(fn table-head-viewer [header-row {:as opts :keys [path]}]
     (let [header-cells (nextjournal.clerk.viewer/desc->values header-row)
           sub-headers (remove nil? (mapcat #(when (vector? %) (second %)) header-cells))
           sub-headers? (seq sub-headers)]
       [:thead
        (into [:tr.print:border-b-2.print:border-black]
              (map-indexed (fn [index cell]
                             (let [header-cell cell
                                   nested? false]
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
                                  [:div v]]))))
              header-cells)
        (when-not (empty? sub-headers)
          (into [:tr.print:border-b-2.print:border-black]
                (map-indexed (fn [idx cell]
                               [:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.border-slate-300.text-center.whitespace-nowrap.border-b
                                cell]))
                sub-headers))])))

(def table-col-bars
  '(fn table-col-bars [{:keys [col-type category-count distribution width height]}]
     (r/with-let [selected-bar (r/atom nil)]
       (let [width 140
             height 30
             last-index (dec (count distribution))]
         [:div
          [:div.text-slate-500.dark:text-slate-400.font-normal
           {:class "text-[12px] h-[24px] leading-[24px]"}
           (if-let [{:keys [count percentage]} @selected-bar]
             (str count " rows (" (.toFixed (* 100 percentage) 2) "%)")
             col-type)]
          (into
           [:div.flex.relative
            {:style {:width width :height height}
             :class "rounded-sm overflow-hidden items-center "}]
           (map-indexed
            (fn [i {:as bar :keys [label count percentage range]}]
              (let [bar-width (* width percentage)]
                [:div.relative.overflow-hidden
                 {:on-mouse-enter #(reset! selected-bar bar)
                  :on-mouse-leave #(reset! selected-bar nil)
                  :class (case label
                           :unique "bg-gray-100 hover:bg-gray-200 dark:bg-gray-800 dark:hover:bg-gray-700 "
                           :empty "bg-orange-200 hover:bg-orange-300 dark:bg-pink-900 dark:bg-opacity-[0.7] dark:hover:bg-pink-800 "
                           "bg-indigo-200 hover:bg-indigo-300 dark:bg-sky-700 dark:hover:bg-sky-500")
                  :style {:width bar-width
                          :height height}}
                 (when (and (contains? #{:unique :empty} label) (< 30 bar-width))
                   [:div.text-slate-500.dark:text-slate-300.font-normal.absolute.left-0.top-0.right-0.bottom-0.flex.items-center.justify-center.whitespace-nowrap
                    {:class "text-[12px]"}
                    (str (.toFixed (* 100 percentage) 2) "%"
                         (when (and (= label :unique) (< 80 bar-width))
                           " unique")
                         (when (and (= label :empty) (< 110 bar-width))
                           " empty/nil"))])
                 (when-not (= i last-index)
                   [:div.absolute.top-0.right-0.bottom-0
                    {:class "bg-white bg-opacity-[0.7] dark:bg-black w-[1px]"}])]))
            distribution))
          [:div.text-slate-500.dark:text-slate-400.font-normal.truncate
           {:class "text-[12px] h-[24px] mt-[1px] leading-[24px] "
            :style {:width width}}
           (if-let [{:keys [count label]} @selected-bar]
             (case label
               :unique (str count " unique values")
               :empty (str count " empty/nil values")
               label)
             (str "(" category-count " categories)"))]]))))

(def table-col-histogram
  '(fn table-col-histogram [{:keys [col-type distribution width height]}]
     (r/with-let [!selected-bar (r/atom nil)
                  fmt (goog.i18n.NumberFormat. (j/get-in goog.i18n.NumberFormat [:Format :COMPACT_SHORT]))]
       (let [max (:count (apply max-key :count distribution))
             last-index (dec (count distribution))
             from (-> distribution first :range first)
             to (-> distribution last :range last)]
         [:div
          [:div.text-slate-500.dark:text-slate-400.font-normal
           {:class "text-[12px] h-[24px] leading-[24px]"}
           (if-let [{:keys [count percentage]} @!selected-bar]
             (str count " rows (" (.toFixed (* percentage 100) 2) "%)")
             col-type)]
          (into
           [:div.flex.relative
            {:style {:width width :height height}}]
           (map-indexed
            (fn [i {:as bar row-count :count :keys [range]}]
              (let [bar-width (/ width (count distribution))
                    selected? (= @!selected-bar bar)
                    last? (= i last-index)]
                [:div.relative.group
                 {:on-mouse-enter #(reset! !selected-bar bar)
                  :on-mouse-leave #(reset! !selected-bar nil)
                  :style {:width bar-width
                          :height (+ height 24)}}
                 [:div.w-full.flex.items-end
                  {:style {:height height}}
                  [:div.w-full.relative
                   {:style {:height (* (/ row-count max) height)}
                    :class "bg-indigo-200 group-hover:bg-indigo-400 dark:bg-sky-700 dark:group-hover:bg-sky-500 "}
                   (when-not last?
                     [:div.absolute.top-0.right-0.bottom-0
                      {:class "bg-white dark:bg-black w-[1px]"}])]]
                 [:div.relative
                  {:class "mt-[1px] h-[1px] bg-slate-300 dark:bg-slate-700"}
                  (when selected?
                    [:div.absolute.left-0.top-0.bg-black.dark:bg-white
                     {:class (str "h-[2px] " (if last? "right-0" "right-[1px]"))}])]
                 (when selected?
                   [:<>
                    [:div.absolute.left-0.text-left.text-slate-500.dark:text-slate-400.font-normal.pointer-events-none
                     {:class "text-[12px] h-[24px] leading-[24px] -translate-x-full"
                      :style {:top height}}
                     (.format fmt (first range))]
                    [:div.absolute.right-0.text-right.text-slate-500.dark:text-slate-400.font-normal.pointer-events-none
                     {:class "text-[12px] h-[24px] leading-[24px] translate-x-full"
                      :style {:top height}}
                     (.format fmt (last range))]])]))
            distribution))
          [:div.text-slate-500.dark:text-slate-400.font-normal.truncate
           {:class "text-[12px] h-[24px] leading-[24px] "
            :style {:width width}}
           (when-not @!selected-bar
             [:div.relative.pointer-events-none
              [:div.absolute.left-0.top-0 (.format fmt from)]
              [:div.absolute.right-0.top-0 (.format fmt to)]])]]))))

#_(def table-summary-sample
  '(defn table-summary-sample [{:keys [continuous?]}]
     (if continuous?
       {:continuous? continuous?
        :col-type "number"
        :distribution [{:range [0 100000]
                        :row-count 1
                        :percentage 0.011}
                       {:range [100000 200000]
                        :row-count 16
                        :percentage 0.18}
                       {:range [200000 300000]
                        :row-count 33
                        :percentage 0.37}
                       {:range [300000 400000]
                        :row-count 22
                        :percentage 0.25}
                       {:range [400000 500000]
                        :row-count 8
                        :percentage 0.09}
                       {:range [500000 600000]
                        :row-count 3
                        :percentage 0.034}
                       {:range [600000 700000]
                        :row-count 3
                        :percentage 0.034}
                       {:range [700000 800000]
                        :row-count 1
                        :percentage 0.011}
                       {:range [800000 900000]
                        :row-count 0
                        :percentage 0.0}
                       {:range [900000 1000000]
                        :row-count 0
                        :percentage 0.0}
                       {:range [1000000 1100000]
                        :row-count 0
                        :percentage 0.0}
                       {:range [1100000 1200000]
                        :row-count 1
                        :percentage 0.011}
                       {:range [1200000 1300000]
                        :row-count 0
                        :percentage 0.0}
                       {:range [1300000 1400000]
                        :row-count 0
                        :percentage 0.0}
                       {:range [1400000 1500000]
                        :row-count 0
                        :percentage 0.0}
                       {:range [1500000 1600000]
                        :row-count 0
                        :percentage 0.0}
                       {:range [1600000 1700000]
                        :row-count 1
                        :percentage 0.011}]}
       {:continuous? continuous?
        :col-type "string"
        :category-count 30
        :distribution [{:label "For Those About To Rock (We Salute You)"
                        :row-count 3
                        :percentage 0.068}
                       {:label "Balls to the Wall"
                        :row-count 2
                        :percentage 0.045}
                       {:label :unique
                        :row-count 28
                        :percentage 0.636}
                       {:label :empty
                        :row-count 11
                        :percentage 0.25}]})))

(def table-col-summary
  (walk/postwalk-replace {'table-col-histogram table-col-histogram
                          'table-col-bars table-col-bars}
                         '(defn table-col-summary [{:as summary :keys [continuous?]}]
                            (let [summary (assoc summary :width 140 :height 30)]
                              (if continuous?
                                [table-col-histogram summary]
                                [table-col-bars summary])))))

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
  {:render-fn table-head-viewer-fn})

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

(def table-viewer
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
                                                                ;; TODO: insert stats rows here
                                                                (map (partial viewer/with-viewer table-row-viewer) rows)))
                                             head (cons (viewer/with-viewer (:name table-head-viewer table-head-viewer) head)))))
             (-> wrapped-value
                 viewer/mark-presented
                 (assoc :nextjournal/width :wide)
                 (assoc :nextjournal/value [(viewer/present wrapped-value)])
                 (assoc :nextjournal/viewer {:render-fn 'nextjournal.clerk.render/render-table-error}))))))

(viewer/reset-viewers! :default
                       (viewer/add-viewers (viewer/get-default-viewers)
                                           [table-viewer]))

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
