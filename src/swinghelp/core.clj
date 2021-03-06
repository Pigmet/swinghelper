(ns swinghelp.core
  (:require [defun.core :refer [defun]]
            [clojure.string :refer [join]]
            [clojure.spec.alpha :as s])
  (:use [seesaw core font]))

(defn get-elem-id [root id]
  (if-let [res (select root [(keyword(str "#" (name id)))])]
    res
    (throw (Exception.
            (format "error from get-elem-id: no elem for %s" id)))))

(defn get-elems-class [root class-id]
  (select root [(keyword (str "." (name class-id)))]))

(defun sget
  ([root ([elem-key component-key] :seq)]
   (-> root (get-elem-id elem-key) (config component-key)))
  ([root ([elem-key] :seq)] (get-elem-id root elem-key))
  ([root elem-key] (recur root [elem-key])))

(defun sset!
  ([root ([elem-key component-key] :seq) v]
   (config! (sget root elem-key) component-key v)
   root))

(defun sget-class
  ([root ([class-key] :seq) ] (get-elems-class root class-key))
  ([root class-key] (sget-class root [class-key]))
  ([root ([class-key component-key] :seq)]
   (map #(config % component-key) (sget-class root class-key))))

(defun sset-class!
  ([root ([class-key component-key] :seq) v]
   (let [elems (sget-class root class-key)]
     (doseq [el elems]
       (config! el component-key v )))
   root))

;; html

(s/def ::html-spec
  (s/or
   :literal (complement coll?)
   :command (s/cat :k keyword? :style (s/? map?) :args (s/* any?))
   :invalid any?))

;;(eduplot.helper/ditch html)

(defmulti ^:private  html*
  "Converts data to html string."
  (fn [x] (key (s/conform ::html-spec x))))

(defmethod html* :invalid [x]
  (throw (Exception.
          (format "html error: don't know how to convert %s." x))))

(defmethod html* :literal [x] (str x))

(s/conform ::html-spec [:p {:text-align "center" :color "black"} "hello"])

(s/conform ::html-spec
           [:html [:p "a"]])

(defn- style-string [m]
  (join ";" (map (fn [[k v]] (format "%s:%s" (name k) v)) m)))

;; FIXME: parsing data then appling html* recursively to the result
;; does not work since the data is now a map like {:k ... :style ..}

(defmethod html* :command [data]
  (let [{:keys [k style args]} (val (s/conform ::html-spec data))
        style-s (when style (style-string style))
        wrap-this (fn [s] (format "<%s>" s))
        beg (cond-> (name k)
              style-s (str  (format " style=\"%s\"" style-s))
              true wrap-this)
        end (wrap-this (str "/" (name k)))
        body-s (join " " (map html* args))]
    (str beg body-s end)))

(defn html
  "Converts args to html string."
  [& args] (html*  (list* :html args)))

(comment

  (defn- demo [s]
    (-> (frame :width 500
               :height 500
               :content (label :id :output :class :font :text s))
        (sset-class! [:font :font] (font :size 30))
        show!))

  (demo (html 
         [:p {:color "red"} "first p"]
         [:p {:color "blue" :text-align "center"
              :background-color "gray"} "second p"]))

  )




