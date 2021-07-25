(ns quil-workflow.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:require [quil-workflow.dynamic :as dynamic]))
    
(q/defsketch example
  :title "BMS"
  :setup dynamic/setup           
  :draw dynamic/draw
  :update dynamic/update-state
  :mouse-moved dynamic/mouse-moved
  :size [323 200]
  :middleware [m/fun-mode])     
