
(ns ml-class.bayes
  (:require [clojure.string :as string]
            [stout.porter-stemmer :as stout]
            [clojure.java.io :as io]))

(def ^:dynamic *feature-count* (ref {}))
(def ^:dynamic *category-count* (ref {}))

;; # Text handling
;;
;; Here we handle the process of taking some text and breaking it
;; apart into only the features that we think will be useful in
;; classifying with.  This does things like standardise the case of
;; the text, remove punctuation, etc...

(defn- ^{:doc "Decides if the word falls inside the length range
  that we consider 'good'.  Words that are too long or too short
  are generally less useful."}
  good-word-length [word]
  (let [length (count word)]
    (and (> length 3)
         (< length 16))))

(defn ^{:doc "Breaks a block of text into it's important features.
  These are the words that we will be storing and using in the 
  probability scoring."}
  features [text]
  (->> (string/split text #"\s+")
       (map #(.toLowerCase %))
       (map #(string/replace % #"[^A-Za-z0-9]" ""))
       (filter good-word-length)
       (map stout/porter-stemmer)))

;; # Training
;;
;; The training function handle consuming text data and using it to
;; score against categories so we can later use that to do matching
;; for text against those same categories.

(defn- ^{:doc "Increments a value if is not nill, otherwise sets
  it to the default value of 1."}
  inc-if [val]
  (if (nil? val) 1
      (inc val)))

(defn- ^{:doc "Uses a chunk of text that is for the specified category
  to add data to our scoring."}
  train-category [category text]
  (doseq [feature (features text)]
    (dosync
      (alter *feature-count*
        #(update-in % [feature category] inc-if))))
  (dosync
    (alter *category-count*
      #(update-in % [category] inc-if))))

(defn ^{:doc "Loads all the training data into memory ready for scoring.
  Use this method if you're playing with the functions from the REPL."}
  train []
  (doseq [category ["arts" "sports"]]
    (doseq [line (line-seq (io/reader (str "intro_web_data/" category)))]
      (train-category category line))))

;; # Classification
;;
;; When the system has been trained with some data we can then use
;; those scores to apply the Naive Bayes algorithm for matching text
;; to trained categories.

(defn- ^{:doc "Returns the features count for a particular category, or
  the default of 0.0 if it has not been scored at all."}
  feature-count [category feature]
  (get-in @*feature-count* [feature category] 0.0))

(defn- ^{:doc "As above, returns the number of times we've scored some
  text for category.  This allows balancing out of results if we've got
  more data trained for one category than another."}
  category-count [category]
  (get-in @*category-count* [category] 0.0))

(defn- ^{:doc "Calculates the probability that a feature matches a
  category.  This is only a simple match before we can then take into
  account a more balanced match looking at scores for other categories."}
  feature-probability [category feature]
  (/ (feature-count category feature)
     (category-count category)))

(defn- ^{:doc "Balances a simple probablity match with a cross-category
  match to try and achieve a more evenly weighted result."}
  weighted-probability [category acc feature]
  (let [weight 1.0
        ap 0.5
        basic-prob (feature-probability category feature)
        feat-total (->> (keys @*category-count*)
                        (map #(feature-count % feature))
                        (reduce + 0))]
    (* acc
       (/ (+ (* weight ap)
             (* feat-total basic-prob))
          (+ weight feat-total)))))

(defn- ^{:doc "Returns weighted probability results for all the features
  in a piece of text."}
  text-probability [text category]
  (reduce (partial weighted-probability category) 1 (features text)))

(defn- ^{:doc "Calculates the probability for a category match based
  on how much training data it has compared to other categories."}
  category-probability [category]
  (/ (get @*category-count* category)
     (reduce #(+ %1 (second %2)) 0 @*category-count*)))

(defn ^{:doc "Calculate a probabalistic match for the text being about
  to the specified category."}
  probability [text category]
  (let [cat-prob (category-probability category)
        txt-prob (text-probability text category)]
    (* txt-prob cat-prob))) 

