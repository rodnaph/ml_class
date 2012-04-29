
(ns ml-class.core
  (:require [ml-class.bayes :as bayes]
            [ml-class.clustering :as cl]))

;; # Actions
;;
;; This section contains the actions available when running this
;; program from Leiningen, for example:
;;
;;   lein run bayes

(defn- ^{:doc "Loads some training data and then runs some simple
  example text through the classifier, printing the results to the
  console."}
  bayes []
  (bayes/train)
  (let [text "Dustin Johnson wound up with another bizarre penalty Thursday when his caddie thought his tee time was 40 minutes later than it was, and he raced to the first tee at the Northern Trust Open in Los Angeles to avoid disqualification. Johnson was given a two-shot penalty for not being on the tee box at his starting time. Players then have five minutes"]
    (println "Arts: " (bayes/probability text "arts"))
    (println "Sports: " (bayes/probability text "sports"))))

(defn- ^{:doc "Uses URL's with associated tags and clusters them into
  similar groups. Picks the 10th URL from the data, and then prints
  the 10 most similar URLs based on tag matching."}
  cluster []
  (let [url (nth (cl/url-data) 10)]
    (println (cl/urls-like url 10))))

(defn- ^{:doc "Prints a usage message"}
  usage []
  (println "Usage: lein run (bayes)"))

;; # Main

(defn ^{:doc "If you're running this through Leiningen then by default it
  just loads the training data and then prints the scores for some matches."}
  -main [action]
  (condp = action
    "bayes" (bayes)
    "cluster" (cluster)
    (usage)))

