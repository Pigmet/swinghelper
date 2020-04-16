(ns swinghelp.core
  (:require [defun.core :refer [defun]])
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


