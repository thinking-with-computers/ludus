(defproject ludus "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [babashka/fs "0.1.6"]
                 [quil "4.0.0-SNAPSHOT-1"]]
>>>>>>> 55d76f6854bf67119873d98e2c9c18d8390ab90a
  :plugins [[lein-cljfmt "0.8.0"]]
  :repl-options {:init-ns ludus.core}
  :main ludus.core
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ["--enable-preview"]
  )
