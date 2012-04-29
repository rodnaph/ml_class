
(ns ml-class.clustering
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.csv :as csv]))

(defn ^{:doc "Creates a sequence from the CSV input.  The sequence
  consists of vectors with two elements [URL TAGS], where the tags
  are a vector of tag strings."}
  urls-seq []
  (map 
    #(vector (first %) 
             (string/split (second %) #","))
    (csv/read-csv 
      (io/reader "intro_web_data/links.csv"))))

(defn ^{:doc "Returns a vector of all distinct tags from the CSV"}
  distinct-tags []
  (->> (urls-seq)
       #(reduce #(reduce conj %1 (second %2)) [])
       (distinct)))

(defn to-url-matches [[url tag-str]]
  (let [url-tags (tags tag-str)]
    (vector url
      (->> (all-tags)
           (map #(if (some #{%} tags) 1 0))))))

(defn url-data []
  (let [tag-data (all-tags)]
    (->> (links-seq)
         (map to-url-matches))))

