import logging
from logging.handlers import TimedRotatingFileHandler

import csv
from pprint import pprint

import socket

import selectors

import time

sel = selectors.DefaultSelector()

def return_servers():
    with open('servers.txt', mode='r') as csv_file:
        csv_reader = csv.DictReader(csv_file)
        line_count = 0
        servers = []
        for row in csv_reader:
            servers.append(row)
        return servers

servers = return_servers()
serverNameLookup = {}
for server in servers:
    serverNameLookup[server['address']] = server['name']

def get_server_name(address):
    return serverNameLookup[address]

def register_logger(daemonType, remoteName):
    hostname = socket.gethostname()
    logName = daemonType + "_" + hostname + "_" + remoteName.replace(":", "-")
    logger = logging.getLogger(remoteName)
    timedRotatingFileHandler = TimedRotatingFileHandler(filename="logs/" + logName)

    logger.addHandler(timedRotatingFileHandler)
    logger.warning("Starting logger " + logName)

    return logger

def get_logger(daemonType, remoteName):
    return logging.getLogger(remoteName)

def service_connection(key, mask):
    sock = key.fileobj
    data = key.data
    logger = get_logger(data.type, data.name)
    if mask & selectors.EVENT_READ:
        recv_data = sock.recv(1024)  # Should be ready to read
        if recv_data:
            try:
                decoded_data = recv_data.decode()
                monotonic_time = time.monotonic_ns()
                logger.warning(f"RECV {monotonic_time}, {decoded_data}")
                print(f"RECV {monotonic_time}, {decoded_data}")
            except UnicodeDecodeError:
                text = str(recv_data)
                print(f"Dados não podem ser traduzidos para UTF-8: {text}")
        if not recv_data:
            logger.warning(f"closing connection TO {sock.getpeername()}")
            sel.unregister(sock)
            sock.close()
    if mask & selectors.EVENT_WRITE:
        monotonic_time = time.monotonic_ns()
        if (data.last_time + data.time_between_pings < monotonic_time):
            monotonic_time = time.monotonic_ns()
            message = f"PING {monotonic_time}"
            sent = sock.send(message.encode())  # Should be ready to write
            print(f"PING {monotonic_time}")
            data.last_time = monotonic_time

class TimedRotatingFileHandlerWithGzip(TimedRotatingFileHandler):
    def __init__(self, filename="", when="midnight", interval=1,
                 backupCount=0, utc=True,  encoding="utf-8"):
        TimedRotatingFileHandler.__init__(
            self,
            filename=filename,
            when=when,
            interval=int(interval),
            backupCount=int(backupCount),
            utc=utc,
            encoding=encoding
        )
        self.suffix = "%Y-%m-%d" # or anything else that strftime will allow

    def doRollover(self):
        super(TimedRotatingFileHandler, self).doRollover()
        log_dir = dirname(self.baseFilename)
        to_compress = [
            join(log_dir, f) for f in listdir(log_dir) if f.startswith(
                basename(splitext(self.baseFilename)[0])
            ) and not f.endswith((".gz", ".log"))
        ]
        for f in to_compress:
            if exists(f):
                with open(f, "rb") as _old, gzip.open(f + ".gz", "wb") as _new:
                    shutil.copyfileobj(_old, _new)
                remove(f)
