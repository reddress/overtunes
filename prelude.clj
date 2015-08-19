;;;; $ lein repl

(use 'overtone.core)
(boot-external-server)

;;;; https://github.com/overtone/overtone/wiki/Pitches-and-Chords

;;;; http://vishnumenon.com/2013/06/25/musical-chains-music-generation-with-clojure/

(definst triangle-wave [freq 440 attack 0.01 sustain 0.18 release 0.2 vol 0.6]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))

(defn tri2 [music-note]
  (triangle-wave (midi->hz (note music-note))))

(defn play-midi-note-at [t note inst]
  (let [time (now)]
    (at (+ time t) (inst note)))
  nil)

(defn play-seq-loop [inst intv notes]
  (loop [notes notes i 0]
    (when (seq notes)
      (play-midi-note-at (* intv i) (first notes) inst)
      (recur (rest notes) (inc i)))))

;;; (play-seq-loop saw2 200 [40 45 50])

(defn prelude-line
  "Given the four-note base, create sequence of notes up to fifth octave
  and back."
  [base-kw]
  (let [letter-map {:g+ 44 :a 45 :a+ 46 :b 47 :C 48 :C+ 49 :D 50 :D+ 51 :E 52
                    :F 53 :F+ 54 :G 55 :G+ 56 :A 57}
        base (map #(letter-map %) base-kw)
        increasing (loop [i 0 result []]
                     (if (= i 4)
                       result
                       (recur (inc i)
                              (concat result (map #(+ (* i 12) %) base)))))]
    (concat increasing [(+ (* 12 4) (first base))]
            (butlast (reverse increasing)))))

;;; (recording-start "~/my-overtone/output/ff-prelude-sustain.wav")
(let [prelude (flatten
               (map prelude-line
                    (conj (into []
                                (take 4 (cycle [[:C :D :E :G] [:a :b :C :E]])))
                          [:a :C :F :G] [:b :D :G :A]
                          [:g+ :C :D+ :G] [:a+ :D :F :A])))]
  (play-seq-loop
   tri2 160 (flatten (repeat 2 prelude))))   
 
;;; (recording-stop)
