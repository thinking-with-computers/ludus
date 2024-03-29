(defproject ludus "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://ludus.dev"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [babashka/fs "0.4.19"]
                 ]
  :plugins [[lein-cljfmt "0.8.0"]]
  :repl-options {:init-ns ludus.core}
  :main ludus.core
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ["--enable-preview"]
  )
