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
   :bgms (list)
   :meter "1"})

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
   :bgms #"(?im)(?<=#)\d{3}01:.+$"
   :meter #"(?im)(?<=#)\d{3}02:.+$"})

(defn parse-data-line [tag line]

  (cond (= tag :bgms)
        (let [measure (Integer/parseInt (apply str (take 3 line)))
              data (drop 6 line)]
          [measure (map #(apply (comp keyword str) %) (partition 2 data))])


        (or (= :p1visibles tag) (= :p2visibles tag) 
            (= :p1invisibles tag) (= :p2invisibles tag))
        (let [measure-column (apply str (take 5 line))
              data (drop 6 line)]
          [measure-column (map #(apply (comp keyword str) %) (partition 2 data))])


        (or (= tag :wavs) (= tag :bmps))
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

(defn combine-bgms-lists [bgms]
  (reduce (fn [coll [key data]] (assoc coll key (map second data))) {} bgms))

(defn first-pass [bms bms-state pattern]
  (let [[tag regex] pattern]
    (cond (or (= :wavs tag) (= :bmps tag) (= :bgms tag)
              (= :p1visibles tag) (= :p2visibles tag)
              (= :p1invisibles tag) (= :p2invisibles tag))
          (->> (or (re-seq regex bms) (list))
               (map #(parse-data-line tag %))
               (apply concat)
               (#(if (= tag :bgms)
                   (combine-bgms-lists
                        (group-by first (partition 2 %)))
                   (apply hash-map %)))
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


(defn measure-timings [initial-time sounds time-per-measure]
  (let [time-per-sound (/ time-per-measure (count sounds))
        sounds-with-indices (filter #(not= :00 (second %)) (map #(vector %1 %2) (range) sounds))]
    (reduce #(into %1 [(+ (* (first %2) time-per-measure) initial-time) 
                       (second %2)]) [] sounds-with-indices)))

(defn timings [measures time-per-measure]
  (reduce #(into %1 (measure-timings (* (dec (key %2)) time-per-measure) 
                                     (val %2) time-per-measure)) [] measures))

(defn time-columns [columns time-per-measure]
  (reduce #(assoc %1 (key %2) (->> (timings (val %2) time-per-measure)
                                   (partition 2)
                                   (sort-by first))) {} columns))

(defn time-bgms [measures time-per-measure]
  (sort-by first (partition 2 (mapcat #(let [initial-time (* (dec (key %1)) time-per-measure)
                                             measure-coll (val %1)]
                                         (mapcat (fn [meas] (measure-timings initial-time meas time-per-measure))
                                                 measure-coll))
                                      measures))))

(defn calculate-offsets [bms-state]
  (let [tags (list :p1visibles :p1invisibles :p2visibles :p2invisibles)
        last-measure (apply max (keys (concat (get bms-state :bgms) (mapcat val (mapcat #(get bms-state %) tags)))))
        bpm (:bpm bms-state)
        meter (:meter bms-state)
        time-per-measure (* (/ meter bpm) 60 1000 last-measure)]
    (->> (->> tags
              (reduce #(assoc %1 %2 (get bms-state %2)) {})
              (reduce #(assoc %1 (key %2) (time-columns (val %2) time-per-measure)) {}))
         (#(assoc %1 :bgms (time-bgms (get bms-state :bgms) time-per-measure)))
         (reduce #(assoc %1 (key %2) (val %2)) bms-state))))


(defn convert-to-numerical [bms-state]
  (let [int-tags (list :playlevel :player :total)
        double-tags (list :meter :bpm)]
    (reduce #(update %1 %2 (fn [number] (Double/parseDouble number)))
            (reduce #(update %1 %2 (fn [number] (Integer/parseInt number))) bms-state int-tags)
            double-tags)))


(defn bms-data [bms]
  (->
   (reduce #(first-pass bms %1 %2) bms-default-state bms-command-patterns)
   convert-to-numerical
   calculate-offsets))
