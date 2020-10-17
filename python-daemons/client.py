#!/usr/bin/env python3.8

import sys
import socket
import selectors
import types

import time

from pprint import pprint

import logging
import gzip

import functools

import common

sel = selectors.DefaultSelector()
events = selectors.EVENT_READ | selectors.EVENT_WRITE

def start_connections(timeout_select):
    servers = common.return_servers()
    pprint(servers)
    for server in servers:
        server_addr = (server['address'], int(server['port']))
        print("starting connection to", server['name'])
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setblocking(False)
        sock.connect_ex(server_addr)
        data = types.SimpleNamespace(
            outb=b"",
            server_addr=server_addr,
            name=server['name'],
            type='client',
            time_between_pings=timeout_select*1000000000,
            last_time=0
        )
        sel.register(sock, events, data=data)
        common.register_logger('client', server['name'])

numberOfArguments = 1+2
if len(sys.argv) != numberOfArguments:
    print("usage:", sys.argv[0], "<timeout_select> <log_file>")
    sys.exit(1)

timeout_select, log_file = float(sys.argv[1]), sys.argv[2]

start_connections(timeout_select)

try:
    while True:
        events = sel.select(timeout=timeout_select)
        if events:
            for key, mask in events:
                common.service_connection(key, mask)
        # Check for a socket being monitored to continue.
        if not sel.get_map():
            break
except KeyboardInterrupt:
    print("caught keyboard interrupt, exiting")
finally:
    sel.close()
