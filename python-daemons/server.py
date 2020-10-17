#!/usr/bin/env python3.8

import sys
import socket
import selectors
import types

import common

sel = selectors.DefaultSelector()

def create_all_loggers():
    servers = common.return_servers()
    for server in servers:
        common.register_logger('server', server['name'])

def accept_wrapper(sock, timeout_select):
    conn, addr = sock.accept()  # Should be ready to read
    name = common.get_server_name(addr[0])
    data = types.SimpleNamespace(
        addr=addr,
        type='server',
        name=name,
        outb=b"",
        time_between_pings=timeout_select*1000000000,
        last_time=0
    )
    logger = common.get_logger(data.type, data.name)
    logger.info("accepted connection from" + name)
    conn.setblocking(False)
    events = selectors.EVENT_READ | selectors.EVENT_WRITE
    sel.register(conn, events, data=data)

numberOfArguments = 1+4
if len(sys.argv) != numberOfArguments:
    print("usage:", sys.argv[0], "<host> <port> <timeout_select> <log_file>")
    sys.exit(1)

host, port, timeout_select, log_file = sys.argv[1], int(sys.argv[2]), float(sys.argv[3]), sys.argv[4]

lsock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
lsock.bind((host, port))
lsock.listen()
print("listening on", (host, port))
lsock.setblocking(False)
sel.register(lsock, selectors.EVENT_READ, data=None)

create_all_loggers()

try:
    while True:
        events = sel.select(timeout=timeout_select)
        for key, mask in events:
            if key.data is None:
                accept_wrapper(key.fileobj, timeout_select)
            else:
                common.service_connection(key, mask)
except KeyboardInterrupt:
    print("caught keyboard interrupt, exiting")
finally:
    sel.close()
