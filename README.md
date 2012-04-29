
# bayes Class - Clojure

Implementation of the machine learning class in Clojure.  Documentation is
generated by Marginalia and available on [Github pages](http://rodnaph.github.com/ml_class).

# Naive Bayesian Analysis

To run the example just use Leiningen specifying the action

```bash
lein run bayes
```

Or to interact with the example from the REPL:

```clojure
(require '[ml-class.bayes :as bayes])
(bayes/train)
(bayes/probability "Some sentence about sports" "sports")
```

# Clustering

To try finding similar URLs...

```clojure
(require '[ml-class.clustering :as cl])
" Pick a URL from the data
(def url (nth (cl/url-data) 10))
(cl/urls-like url 10)
```

Or the example from Leiningen:

```bash
lein run cluster
```

