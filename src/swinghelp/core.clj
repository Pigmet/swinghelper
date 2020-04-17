(ns swinghelp.core
  (:require [defun.core :refer [defun]]
            [clojure.string :refer [join]])
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

(defn- decorator [k s]
  (let [beg (format "<%s>" (name k))
        end (format "</%s>" (name k))]
    (str beg s end)))

(s/def ::html-spec
  (s/or
   :literal (complement coll?)
   :command (s/cat :k keyword? :args (s/* ::html-spec))
   :invalid any?))

;;(eduplot.helper/ditch html)

(defmulti html*
  "Converts data to html string."
  (fn [x] (key (s/conform ::html-spec x))))

(defmethod html* :invalid [x]
  (throw (Exception.
          (format "html error: don't know how to convert %s." x))))

(defmethod html* :literal [x] (str x))

(defmethod html* :command [[k & args]]
  (decorator k (join " " (map html* args))))

(defn html [& args] (html* (list* :html args)))
