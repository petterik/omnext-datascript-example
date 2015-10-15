(defproject om-tutorial "0.1.0-SNAPSHOT"
  :description "Om.next + datascript example"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.145"]
                 [org.omcljs/om "1.0.0-alpha2-SNAPSHOT"]
                 [figwheel-sidecar "0.4.0" :scope "provided"]
                 [sablono "0.3.6"]
                 [datascript "0.13.1"]]
  :plugins [[lein-figwheel "0.4.1"]])
