#!/bin/bash

if [ -f "./config/rebar3" ]; then
    exit
fi

cd config
wget https://github.com/erlang/rebar3/releases/download/3.15.1/rebar3
chmod +x rebar3
cd ..