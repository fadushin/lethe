#!/bin/sh

# PROVIDE: lethe
# REQUIRE: network

. /etc/rc.subr

name="lethe"
rcvar=`set_rcvar`
command="/opt/lethe/bin/${name}-ctl"
command_args="start"
# pidfile=/var/tmp/${name}.pid

load_rc_config $name
run_rc_command "$1"

