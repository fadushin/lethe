#!/bin/sh

# TARGET_DIR=/work/eclipse/workspace/3.4/lethe-messaging-server-erlang/erlang-server/ebin
TARGET_DIR=target/test
BASE_DIR=$(pwd)

cd ${TARGET_DIR}

rm -rf content
mkdir -p ./content
mkdir -p ./logs

cd content
ln -s /opt/yaws/var/yaws/www/jsolait
ln -s ${BASE_DIR}/src/main/html/index.html
cd ..

exec erl -boot start_sasl -pa . -pa /opt/yaws/lib/yaws/ebin/ -s net_dushin_lethe_launcher start1 -yaws embedded true


# application:start(net_dushin_lethe).
