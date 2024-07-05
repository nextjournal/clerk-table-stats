;; # 🐞Missing ID Repro
(ns missing-id-repro
  {:nextjournal.clerk/no-cache true}
  (:require [nextjournal.clerk :as clerk]))

(def pred-viewer
  {:pred :a/b
   :transform-fn (fn [{:as wv :keys [id path]}]
                   (prn :matching id :path path)
                   #_ (when-not id (throw (ex-info "No ID" {:wrapped-value wv})))
                   wv)
   :render-fn '(fn [_ _] [:em "test"])})

(clerk/add-viewers! [pred-viewer])

;; cell-id is only present for values matching top-level forms, all matching values
;; at deeper paths won't have an id.

{:a/b 1
 :children [{:a/b 2}]}

(comment
  (-> (ex-data *e) :wrapped-value :path))
