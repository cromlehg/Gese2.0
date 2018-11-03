#!/bin/bash
iptables -t nat -I OUTPUT -o lo -p tcp --dport 80 -j REDIRECT --to-port 8080
iptables -A PREROUTING -t nat -p tcp --dport 80 -m addrtype --dst-type LOCAL -j REDIRECT --to-port 8080

