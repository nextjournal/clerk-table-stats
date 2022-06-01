(ns ^:nextjournal.clerk/no-cache table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk-table-stats :as table-stats]
            [meta-csv.core :as csv]))

(clerk/add-viewers! [table-stats/table+stats-viewer])

^{:nextjournal.clerk/visibility :hide}
(clerk/with-viewer {:render-fn '(fn []
                                  (defn table-col-bars [{:keys [col-type category-count distribution width height]}]
                                    (reagent/with-let [selected-bar (reagent/atom nil)]
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
                                            (str "(" category-count " categories)"))]])))

                                  (defn table-col-histogram [{:keys [col-type distribution width height]}]
                                    (reagent/with-let [!selected-bar (reagent/atom nil)
                                                 fmt identity #_(goog.i18n.NumberFormat. (j/get-in goog.i18n.NumberFormat [:Format :COMPACT_SHORT]))]
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
                                                      (first range)
                                                      #_(.format fmt (first range))]
                                                     [:div.absolute.right-0.text-right.text-slate-500.dark:text-slate-400.font-normal.pointer-events-none
                                                      {:class "text-[12px] h-[24px] leading-[24px] translate-x-full"
                                                       :style {:top height}}
                                                      (last range)
                                                      #_(.format fmt (last range))]])]))
                                             distribution))
                                         [:div.text-slate-500.dark:text-slate-400.font-normal.truncate
                                          {:class "text-[12px] h-[24px] leading-[24px] "
                                           :style {:width width}}
                                          (when-not @!selected-bar
                                            [:div.relative.pointer-events-none
                                             [:div.absolute.left-0.top-0 from #_(.format fmt from)]
                                             [:div.absolute.right-0.top-0 to #_(.format fmt to)]])]])))

                                  (defn table-col-summary [{:as summary :keys [continuous?]}]
                                    (let [summary (assoc summary :width 140 :height 30)]
                                      (if continuous?
                                        [table-col-histogram summary]
                                        [table-col-bars summary])))

                                  (v/html [:div "Hello"]))}
  {:testalizer 123})

(clerk/table {:head [:first-name :last-name :age]
              :rows [["Suzy" "McGyver" 30]
                     ["Frank" "Rottenreiter" 12]]})