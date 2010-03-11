#!/bin/bash

java -cp "clojure.jar:clojure-contrib.jar:." clojure.main -e "(require 'morsebraille)(morsebraille/main)" morsebraille.clj