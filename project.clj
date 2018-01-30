(defproject symbolic-algebra "0.5.1"
  :description "symbolic algebra library with numerical tower"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/test.generative "0.5.2"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.specs.alpha "0.1.24"]]
  :main ^:skip-aot symbolic-algebra.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
