(ns quil-workflow.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m])) 

(def min-r 10)

(defn setup []
  ; initial state
  {:x 0 :y 0 :r min-r})

(defn draw [state]
  (q/background 255)
  (q/ellipse (:x state) (:y state) (/ (:r state) 2) (:r state)))

(defn update [state]
  ; increase radius of the circle by 1 on each frame
  (update-in state [:r] inc))

(defn shrink [r]
  (max min-r (dec r)))

(defn mouse-moved [state event]
  (-> state
      ; set circle position to mouse position
      (assoc :x (:x event) :y (:y event))
      ; decrease radius
      (update-in [:r] shrink)))

