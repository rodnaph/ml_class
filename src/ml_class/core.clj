
(ns ml-class.core
  (:require [clojure.string :as string]
            [stout.porter-stemmer :as stout]
            [clojure.java.io :as io]))

(def ^:dynamic *feature-count* (ref {}))
(def ^:dynamic *category-count* (ref {}))

;; Text handling

(defn- features [text]
  (->> (string/split text #"\s+")
       (map #(.toLowerCase %))
       (map stout/porter-stemmer)))

(defn- feature-count [category feature]
  (get-in @*feature-count* [feature category] 0.0))

(defn- category-count [category]
  (get-in @*category-count* [category] 0.0))

;; Training

(defn- inc-if
  [val]
  (if (nil? val) 1
      (inc val)))

(defn- train-category [category text]
  (doseq [feature (features text)]
    (dosync
      (alter *feature-count*
        #(update-in % [feature category] inc-if))))
  (dosync
    (alter *category-count*
      #(update-in % [category] inc-if))))

(defn train []
  (doseq [category ["arts" "sports"]]
    (doseq [line (line-seq (io/reader (str "intro_web_data/" category)))]
      (train-category category line))))

;; Classification

(defn- feature-probability [category feature]
  (/ (feature-count category feature)
     (category-count category)))

(defn- weighted-probability [category acc feature]
  (let [weight 1.0
        ap 0.5
        basic-prob (feature-probability category feature)
        feat-total (->> (keys @*category-count*)
                        (map #(feature-count % feature))
                        (reduce + 0))]
    (/ (+ (* weight ap)
          (* feat-total basic-prob))
       (+ weight feat-total))))

(defn- text-probability [text category]
  (reduce (partial weighted-probability category) 0 (features text)))

(defn- category-probability [category]
  (/ (get @*category-count* category)
     (reduce #(+ %1 (second %2)) 0 @*category-count*)))

(defn probability [text category]
  (let [cat-prob (category-probability category)
        txt-prob (text-probability text category)]
    (* txt-prob cat-prob))) 

;; Main

(defn -main []
  (train)
  (println "Arts: " (probability "Early Friday afternoon, the lead negotiators for the N.B.A. and the players union will hold a bargaining session in Beverly Hills — the latest attempt to break a 12-month stalemate on a new labor deal." "arts"))
  (println "Sports: " (probability "Early Friday afternoon, the lead negotiators for the N.B.A. and the players union will hold a bargaining session in Beverly Hills — the latest attempt to break a 12-month stalemate on a new labor deal." "sports")))

