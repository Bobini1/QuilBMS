(ns quil-workflow.sound-manager
  (:require [clj-audio.core :as audio]))

(defn load-sound [path]
  (audio/->stream path))

(defn play-sound [sound]
  (audio/play sound))
