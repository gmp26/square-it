#!/bin/bash

cd ~/clojure/square-it
lein clean
lein cljsbuild once min
rsync -av resources/public/* gmp26@maths.org:/www/nrich/html/square-it
