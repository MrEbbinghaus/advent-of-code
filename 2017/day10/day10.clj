(def my-input [97 167 54 178 2 11 209 174 119 248 254 0 255 1 64 190])
(def my-input2 "97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190")

(defn f-on-wrapping-sublist [f l pos len]
  (let [target-zone (f (take len (drop pos (cycle l))))
        rest-zone (take (- (count l) len) (drop (+ pos len) (cycle l)))]
    (take (count l) (drop (- (count l) pos) (cycle (concat target-zone rest-zone))))))

(defn sparse [{:keys [circle-list position skip-size]} length]
    {:circle-list (f-on-wrapping-sublist reverse circle-list position length)
     :skip-size (inc skip-size)
     :position (mod (+ position length skip-size) (count circle-list))})

(defn task1 [input]
  (reduce * (take 2 (:circle-list
                      (reduce sparse {:circle-list (range 256)
                                      :skip-size 0
                                      :position 0}
                                     my-input)))))

(task1 my-input)

;;
;; Task 2
;;

(defn task2 [input]
  (->> (reduce sparse
          {:circle-list (range 256)
           :skip-size 0
           :position 0}
          (flatten (repeat 64 (concat (map int input) [17 31 73 47 23]))))
    :circle-list
    (partition 16)
    (map #(apply bit-xor %))
    (map #(format "%02x" %))
    (apply str)))


(task2 my-input2)

