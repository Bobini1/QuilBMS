(ns quil-workflow.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-workflow.sound-manager :as sound]
            [quil-workflow.parsing :as parsing])) 


(defn setup []
  ; initial state
  {:current-bms (parsing/bms-data parsing/bms)
   :gameplay-state {:note-positions (list (list 100 100))}
   :gameplay-configs {:note-h 5
                      :note-w 10
                      :note-color [10 0 90]
                      :column-mapping {1 1, 2 2, 3 3, 4 4, 5 5, 6 8, 7 9, 8 6}
                      :bg-color [47 79 79]}
   :current-state :gameplay})

(defn draw [state]
  (cond (= (:current-state state) :gameplay)
        (let [bg-color (get-in state [:gameplay-configs :bg-color])
              notes (get-in state [:gameplay-state :note-positions] (list))
              note-w (get-in state [:gameplay-configs :note-w])
              note-h (get-in state [:gameplay-configs :note-h])]
          (do (apply q/background bg-color)
              (run! (fn [[x y]] (q/rect x y note-w note-h)) notes)))
        :else nil))

(defn update-state [state]
  state)

(defn mouse-moved [state event]
  nil)

