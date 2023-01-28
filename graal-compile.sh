#!/opt/homebrew/bin/fish

jenv shell graalvm64-19.0.2

lein uberjar

native-image --report-unsupported-elements-at-runtime --initialize-at-build-time -jar ./target/ludus-0.1.0-SNAPSHOT-standalone.jar -H:Name=./target/ludus
