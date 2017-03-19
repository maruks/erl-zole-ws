#!/bin/bash

CLIENT=~/Projects/zole-gui

lein clean; lein cljsbuild once min

cd -

cp $CLIENT/resources/public/js/compiled/zole.js ./priv/js/

cp $CLIENT/resources/public/css/*.min.css ./priv/css/

cp $CLIENT/resources/public/images/* ./priv/images/
