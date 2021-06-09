(ns quil-workflow.parsing
  (:require [quil-workflow.sound-manager :as sound]))

(def bms (slurp "C:\\Users\\PC\\PycharmProjects\\PygameBMS\\consider nothing\\_considernothing7_5.bme" :encoding "Shift_JIS"))

(def path "C:\\Users\\PC\\PycharmProjects\\PygameBMS\\consider nothing\\")

(def bms-default-state
  {:player 1
   :genre "No genre"
   :title "Untitled"
   :subtitle ""
   :artist "No artist"
   :subartist ""
   :bpm 60
   :playlevel 1
   :total 300
   :stagefile nil
   :rank 2
   :banner nil
   :difficulty 1
   :wavs (list)
   :bmps (list)
   :p1visibles (list)
   :p2visibles (list)
   :p1invisibles (list)
   :p2invisibles (list)
   :bgabase nil
   :bgapoor nil
   :bgm (list)
   :meter 1})

(def bms-command-patterns
  {:player #"(?im)(?<=#PLAYER )[1-4]$"
   :genre #"(?im)(?<=#GENRE ).+$"
   :title #"(?im)(?<=#TITLE ).+$"
   :subtitle #"(?im)(?<=#SUBTITLE ).+$"
   :artist #"(?im)(?<=#ARTIST ).+$"
   :subartist #"(?im)(?<=#SUBARTIST ).+$"
   :bpm #"(?im)(?<=#BPM )\d+(\.\d+)?$"
   :playlevel #"(?im)(?<=#PLAYLEVEL )\d+(\.\d+)?$"
   :total #"(?im)(?<=#TOTAL )\d+(\.\d+)?$"
   :stagefile #"(?im)(?<=#STAGEFILE ).+$"
   :rank #"(?im)(?<=#RANK )[0-3]$"
   :banner #"(?im)(?<=#BANNER ).+$"
   :difficulty #"(?im)(?<=#DIFFICULTY ).+$"
   :wavs #"(?im)(?<=#WAV)\w\w .+$"
   :bmps #"(?im)(?<=#BMP)\w\w .+$"
   :p1visibles #"(?im)(?<=#)\d{3}1[1-9]:.+$"
   :p2visibles #"(?im)(?<=#)\d{3}2[1-9]:.+$"
   :p1invisibles #"(?im)(?<=#)\d{3}3[1-9]:.+$"
   :p2invisibles #"(?im)(?<=#)\d{3}4[1-9]:.+$"
   :bgabase #"(?im)(?<=#)\d{3}04:.+$"
   :bgapoor #"(?im)(?<=#)\d{3}06:.+$"
   :bgm #"(?im)(?<=#)\d{3}01:.+$"
   :meter #"(?im)(?<=#)\d{3}02:.+$"})

(defn parse-data-line [tag line]

  (cond (= tag :bgm)
        (let [measure (Integer/parseInt (Integer. (apply str (take 3 line))))
              data (drop 6 line)]
          [measure {:data (map #(apply (comp keyword str) %) (partition 2 data))}])


        (or (= :p1visibles tag) (= :p2visibles tag) 
            (= :p1invisibles tag) (= :p2invisibles tag))
        (let [measure-column (apply str (take 5 line))
              data (drop 6 line)]
          [measure-column (map #(apply (comp keyword str) %) (partition 2 data))])


        (or (= tag :wavs) (= tag :bms))
        (let [identifier (apply (comp keyword str) (take 2 line))
              data (apply str (drop 3 line))]
          [identifier data])
        :else nil))

(defn split-to-columns [notes]
  (reduce
   (fn [coll [measure-column data]]
      (let [measure (Integer/parseInt (apply str (take 3 measure-column)))
            column (Integer/parseInt (str (nth measure-column 4)))]
        (assoc-in coll [column measure] data))) {} notes))

(defn first-pass [bms bms-state pattern]
  (let [[tag regex] pattern]
    (cond (or (= :wavs tag) (= :bmps tag) (= :bgms tag)
              (= :p1visibles tag) (= :p2visibles tag)
              (= :p1invisibles tag) (= :p2invisibles tag))
          (->> (or (re-seq regex bms) (list))
               (map #(parse-data-line tag %))
               (apply concat)
               (apply hash-map)
               (#(cond (or (= :p1visibles tag) (= :p2visibles tag)
                           (= :p1invisibles tag) (= :p2invisibles tag))
                       (split-to-columns %)
                       
                       (= :wavs tag)
                       (reduce (fn [coll [identifier file]] 
                                 (assoc coll identifier (sound/load-sound (str path file))))
                               {} %)
                       
                       :else
                       (identity %)))
               (assoc bms-state tag))

          :else
          (->> (or (let [search (re-find regex bms)]
                     (if (coll? search) (first search) search)) 
                   (tag bms-state))
               (assoc bms-state tag)))))

(defn calculate-offsets [bms-state]
  (let [tags (list :p1visibles :p1invisibles :p2visibles :p2invisibles :bgms)
        last-measure (apply max (concat))]
      (reduce #() tags)))


(defn bms-data [bms]
  (reduce #(first-pass bms %1 %2) bms-default-state bms-command-patterns))
