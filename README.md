
# ML Class - Clojure

Implementation of the machine learning class in Clojure.

# Simple Usage

To run the example just use Leiningen:

```bash
lein run
```

Or to interact with the example from the REPL:

```clojure
(require '[ml-class.core :as ml])
(ml/train)
(ml/probability "Some sentence about sports" "sports")
```

