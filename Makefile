ERLC=erlc
FLAGS=-W #+debug_info

ERL=erl

USER=$(shell whoami)

HOST=$(shell hostname)


MASTER_NAME=kademliaMaster
MASTER_NODE='${MASTER_NAME}@${HOST}'

MONITOR_NAME=kademliaMonitor
MONITOR_NODE='${MONITOR_NAME}@${HOST}'
.SUFFIXES: .erl .beam .hrl

.erl.beam:
	${ERLC} ${FLAGS} $<

MODS = graphml kademliaGlobal master metric node routingTable systemMonitor utils 

MASTER_DEPENDENCIES = graphml.beam kademliaGlobal.beam master.beam metric.beam node.beam routingTable.beam systemMonitor.beam utils.beam

all: compile

test.beam: test.erl
	${ERLC} ${FLAGS} $<

compile: ${MODS:%=%.beam}

master: ${MASTER_DEPENDENCIES}
	${ERL} -name ${MASTER_NAME} -setcookie ${USER} -eval "master:start(${MONITOR_NODE})."

monitor: ${MASTER_DEPENDENCIES}
	${ERL} -name ${MONITOR_NAME} -setcookie ${USER} -eval "systemMonitor:start(${MASTER_NODE})."
	
clean:
	rm -rf *.beam erl_crash.dump
