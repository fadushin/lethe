#!/bin/sh

TARGET_DIR=/work/eclipse/workspace/3.4/lethe-messaging-server-erlang/erlang-server/ebin

cd ${TARGET_DIR}

mkdir -p ./content
mkdir -p ./logs
echo "Hello World" > ./content/index.html

exec erl -boot start_sasl -pa . -pa /opt/yaws/lib/yaws/ebin/ -yaws embedded true


# application:start(net_dushin_lethe).
