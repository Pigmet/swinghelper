(ns swinghelp.core-test
  (:require [clojure.test :refer :all]
            [swinghelp.core :refer :all])
  (:use [seesaw core font color graphics]))





(def f (frame :width 400
              :height 400
              :content (horizontal-panel
                        :items
                        [(label :text "a" :class :font)])))

(comment
  (-> f
      (sset-class! [:font :font] (font :size 30))
      show!))




