#!/bin/bash

export ERL_LIBS=./_build/default/lib/zole_ws:./_build/default/lib/cowboy:./_build/default/lib/cowlib:./_build/default/lib/jsx:./_build/default/lib/ranch:./_build/default/lib/zole:./_build/default/lib/goldrush:./_build/default/lib/lager

erl -config app.config -run zole_ws_app
