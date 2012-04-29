
(ns ml-class.clustering
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.csv :as csv]))

;; # Data Access
;;
;; The first section just deals with loading the data from the CSV
;; source and presenting it as a nice sequence.

(defn ^{:doc "Creates a sequence from the CSV input.  The sequence
  consists of vectors with two elements [URL TAGS], where the tags
  are a vector of tag strings."}
  urls-seq []
  (map 
    #(vector (first %) 
             (string/split (second %) #","))
    (csv/read-csv 
      (io/reader "intro_web_data/links.csv"))))

;; # URL Tags
;;
;; Just a handy function for extracting all the distinct tags from
;; the CSV.  There is also a memoized version as this is neatly cacheable.

(defn ^{:doc "Returns a vector of all distinct tags from the CSV"}
  distinct-tags []
  (->> (urls-seq)
       (reduce #(reduce conj %1 (second %2)) [])
       (distinct)))

(def ^{:doc "Memoizes the distinct tags function as this will only
  change each time we refresh the dataset."}
  memo-distinct-tags (memoize distinct-tags))

;; # Distance Calculations
;;
;; Here we provide some calculations for finding the distance
;; between vectors (based on tag matches for the URL)

(defn ^{:doc "Euclidean distance between two collections considered
  as coordinates.  Code modified from clojuredocs.org."}
  distance [c1 c2]
  (->> (map - c1 c2) 
       (map #(* % %)) 
       (reduce +)
       (Math/sqrt)))

;; # URL Matching
;;
;; These functions then use the tags and distance calculations to
;; do some matching of URLs, for example providing a function to find
;; URL's in the dataset similar to a specified one.

(defn ^{:doc "Takes a URL/tag pair and changes it to a pair consisting
  of the URL and a vector of 1/0 matches for each distinct tag.  This
  vector of matches can then be used to compute the distance between
  different URL pairs."}
  url-to-tag-matches [[url url-tags]]
  (vector url
    (->> (memo-distinct-tags)
         (map #(if (some #{%} url-tags) 1 0)))))

(defn ^{:doc "Returns URL/coords pairs for the dataset"}
  url-data []
  (->> (urls-seq)
       (map url-to-tag-matches)))

(defn ^{:doc "Take a URL vector and return the total most similar
  urls in the data-set."}
  urls-like [x total]
  (->> (url-data)
       (map #(vector (first %)
                     (distance (second x) (second %))))
       (sort #(< (second %1) (second %2)))
       (take total)))

