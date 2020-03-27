#!/bin/bash

INET_STR=`ip addr show dev br0 | grep "inet\ " | awk '{print $2}'`
echo $INET_STR
CIDR=`echo $INET_STR | awk -F'/' '{print $2}'`
echo $CIDR
IP_ADDR=`echo $INET_STR | awk -F'/' '{print $1}'`
echo $IP_ADDR
GW=`ip route | grep ^default | awk '{print $3}'`
echo $GW

solo5-hvt --net:service=tap100 __blog.hvt \
	--ipv4=$INET_STR \
	--ipv4-gateway=$GW \
	--remote=git://github.com/dinosaure/blog.x25519.net \
	--port 80 \
	-l debug | tee blog.log
