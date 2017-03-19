#!/bin/bash

CLIENT=~/Projects/zole-gui

cd $CLIENT

lein clean; lein cljsbuild once min
lein minify-assets

cd -

mkdir -p ./priv/js
mkdir -p ./priv/images
mkdir -p ./priv/css

cp $CLIENT/resources/public/js/compiled/zole.js ./priv/js/

cp $CLIENT/resources/public/css/*.min.css ./priv/css/

cp $CLIENT/resources/public/images/* ./priv/images/
