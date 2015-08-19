;;;; $ lein repl

(use 'overtone.core)
(boot-external-server)

;;;; https://github.com/overtone/overtone/wiki/Pitches-and-Chords

(definst saw-wave [freq 440 attack 0.01 sustain 0.22 release 0.32 vol 0.4]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn saw2 [music-note]
  (if (> music-note 0)
    (saw-wave (midi->hz (note music-note)))))

(definst triangle-wave [freq 440 attack 0.01 sustain 0.16 release 0.15 vol 0.75]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))

(defn tri2 [music-note]
  (if (> music-note 0)
    (triangle-wave (midi->hz (note music-note)))))

(defn play-note-at [t note inst]
  (let [letter-map {:zb 47 :c 48 :c+ 49 :d 50 :d+ 51 :e 52 :f 53 :f+ 54 :g  55
                    :g+ 56 :a 57 :a+ 58 :b 59 :C 60 :C+ 61 :D 62 :D+ 63 :E 64
                    :F 65 :F+ 66 :G 67 :G+ 68 :A 69 :A+ 70 :B 71 :r 0
                    :aC 72 :aC+ 73 :aD 74 :aD+ 75}
        time (now)]
    (at (+ time t) (when-not (= note :r) (inst (letter-map note))))))

(defn play-seq [inst intv & notes]
  (map-indexed #(play-note-at (* intv %1) %2 inst) notes))

(comment
;;;; awful timing
(play-seq saw2 190 :A+ :r :G+ :r   :A+ :r :G+ :aC+ :r :aC+ :aC :r  :aC+ :aC :r :aC :A+ :r :G+ :r :G :r :G+ :F)
(play-seq saw2 190 :G  :r :F  :r   :G  :r :F  :F   :r :F   :D+ :r  :F   :D+ :r :D+ :G :r  :F  :r :D+ :r :F :C+)
(play-seq saw2 190 :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :c+ :g+ :c+ :g+ :c+ :g+ :c+ :g+ :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :c+ :g+ :c+ :g+ :c+ :g+ :c+ :g+)
)

(defn play-chord [insts note-kws]
  (let [letter-map {:zb 47 :c 48 :c+ 49 :d 50 :d+ 51 :e 52 :f 53 :f+ 54 :g  55
                    :g+ 56 :a 57 :a+ 58 :b 59 :C 60 :C+ 61 :D 62 :D+ 63 :E 64
                    :F 65 :F+ 66 :G 67 :G+ 68 :A 69 :A+ 70 :B 71 :r 0
                    :aC 72 :aC+ 73 :aD 74 :aD+ 75}]
    ;;;; (map #(%1 (letter-map %2)) insts note-kws)))
    (loop [insts insts note-kws note-kws]
      (when (seq insts)
        ((first insts) (letter-map (first note-kws)))
        (recur (rest insts) (rest note-kws))))))

(defn play-chord-at [t insts note-kws]
  (let [time (now)]
    (at (+ time t) (play-chord insts note-kws))))

(comment
(play-chord-at 0 [saw2 saw2 tri2] [:A+ :G :d+])
(play-chord-at 200 [saw2 saw2 tri2] [:r :r :a+])
(play-chord-at 400 [saw2 saw2 tri2] [:G+ :F :d+])
(play-chord-at 600 [saw2 saw2 tri2] [:r :r :a+])
(play-chord-at 800 [saw2 saw2 tri2] [:A+ :G :d+])
(play-chord-at 1000 saw2 :r :r :a+)
(play-chord-at 1200 saw2 :G+ :F :d+)
(play-chord-at 1400 saw2 :aC+ :F :a+)
(play-chord-at 1600 saw2 :r :r :d+)
(play-chord-at 1800 saw2 :aC+ :F :a+)
)

(defn play-voices [insts intv & voices]
  (map-indexed #(play-chord-at (* intv %1) insts %2) (apply map vector voices)))


;;; (play-voices saw2 1200 [:c :e :g] [:e :r :b] [:g :r :d])

;;; (recording-start "~/my-overtone/output/ff-fanfare-sustain.wav")
(play-voices [saw2 saw2 tri2] 200
             [:A+ :r :G+ :r   :A+ :r :G+ :aC+ :r :aC+ :aC :r  :aC+ :aC :r :aC :A+ :r :G+ :r :G :r :G+ :F  :r :r :r :r :r :r :r :r
              :A+ :r :G+ :r   :A+ :r :G+ :aC+ :r :aC+ :aC :r  :aC+ :aC :r :aC :A+ :r :G+ :r :A+ :r :aC+ :aD+  :r :r :r :r :r :r :r :r
              :A+ :r :G+ :r   :A+ :r :G+ :aC+ :r :aC+ :aC :r  :aC+ :aC :r :aC :A+ :r :G+ :r :G :r :G+ :F  :r :r :r :r :r :r :r :r
              :A+ :r :G+ :r   :A+ :r :G+ :aC+ :r :aC+ :aC :r  :aC+ :aC :r :aC :A+ :r :G+ :r :A+ :r :aC+ :aD+  :r :r :r :r :r :r :r :r]
             [:G  :r :F  :r   :G  :r :F  :F   :r :F   :D+ :r  :F   :D+ :r :D+ :G :r  :F  :r :D+ :r :F :C+ :r :r :r :r :r :r :r :r
              :G  :r :F  :r   :G  :r :F  :F   :r :F   :D+ :r  :F   :D+ :r :D+ :G :r  :F  :r :G :r :A+ :B :r :r :r :r :r :r :r :r
              :G  :r :F  :r   :G  :r :F  :F   :r :F   :D+ :r  :F   :D+ :r :D+ :G :r  :F  :r :D+ :r :F :C+ :r :r :r :r :r :r :r :r
              :G  :r :F  :r   :G  :r :F  :F   :r :F   :D+ :r  :F   :D+ :r :D+ :G :r  :F  :r :G :r :A+ :B :r :r :r :r :r :r :r :r]
             [:d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :c+ :g+ :c+ :g+ :c+ :g+ :c+ :g+ :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :c+ :g+ :c+ :g+ :c+ :g+ :c+ :g+
              :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :c+ :g+ :c+ :g+ :c+ :g+ :c+ :g+ :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :zb :f+ :zb :f+ :zb :f+ :zb :f+
              :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :c+ :g+ :c+ :g+ :c+ :g+ :c+ :g+ :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :c+ :g+ :c+ :g+ :c+ :g+ :c+ :g+
              :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :c+ :g+ :c+ :g+ :c+ :g+ :c+ :g+ :d+ :a+ :d+ :a+ :d+ :a+ :d+ :a+ :zb :f+ :zb :f+ :zb :f+ :zb :f+])

;;; (recording-stop)
