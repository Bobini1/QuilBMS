(ns quil-workflow.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:require [quil-workflow.dynamic :as dynamic]))
    
(q/defsketch example                
  :title "Oh so many grey circles"
  :setup dynamic/setup           
  :draw dynamic/draw
  :update dynamic/update
  :mouse-moved dynamic/mouse-moved
  :size [323 200]
  :middleware [m/fun-mode])     
