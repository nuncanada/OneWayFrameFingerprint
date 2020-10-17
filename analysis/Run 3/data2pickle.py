import csv
import time
import gzip

from datetime import datetime
from pprint import pprint

import sys
import glob
import pickle

import os
import ntpath

runDirectory = 'E:\\dados_experimento\\Run 3\\'

cloudServers = {
      '10.150.0.2': 'ashburn',
      '10.168.0.2': 'losangeles',
      '10.162.0.2': 'quebec',
      '10.158.0.2': 'saopaulo',
      '10.138.0.2': 'thedalles'
}

def path_leaf(path):
    head, tail = ntpath.split(path)
    return tail or ntpath.basename(head)

def extractUnidirectionalPingData(gzippedLog):
    unidirectionalPingData={}
    billion = 1000000000

    with gzip.open(gzippedLog, 'rt') as f:
        reader = csv.reader(f)
        i=0
        for row in reader:
            i=i+1

            if len(row) > 9:
                pongIp = row[4]

                pongServerName = cloudServers[pongIp]

                receiverTime = row[0][5:]
                receiverIp = row[1]
                recvRawClock = row[2]
                pongTime = row[3][5:]
                pongRawClock = row[5]
                pingTime = row[6][5:]
                pingIp = row[8]
                pingRawClock = row[9]

                recv = receiverTime.split(".")
                pong = pongTime.split(".")
                ping = pingTime.split(".")

                recvDateTime = recv[0]
                pongDateTime = pong[0]
                pingDateTime = ping[0]

                recvSplitNanoSecondsTZ = recv[1].split(" ")
                pongSplitNanoSecondsTZ = pong[1].split(" ")
                pingSplitNanoSecondsTZ = ping[1].split(" ")

                recvNanoSeconds = recvSplitNanoSecondsTZ[0] + ("0" * (9 - len(recvSplitNanoSecondsTZ[0])))
                pongNanoSeconds = pongSplitNanoSecondsTZ[0] + ("0" * (9 - len(pongSplitNanoSecondsTZ[0])))
                pingNanoSeconds = pingSplitNanoSecondsTZ[0] + ("0" * (9 - len(pingSplitNanoSecondsTZ[0])))

                formatoSemNano = "%Y-%m-%d %H:%M:%S"
                recvDateTime_struct = datetime.strptime(recvDateTime, formatoSemNano)
                pongDateTime_struct = datetime.strptime(pongDateTime, formatoSemNano)
                pingDateTime_struct = datetime.strptime(pingDateTime, formatoSemNano)

                recvTimestamp = (int(time.mktime(recvDateTime_struct.timetuple())) * billion) + int(recvNanoSeconds)
                pongTimestamp = (int(time.mktime(pongDateTime_struct.timetuple())) * billion) + int(pongNanoSeconds)
                pingTimestamp = (int(time.mktime(pingDateTime_struct.timetuple())) * billion) + int(pingNanoSeconds)

                pongRawClockSplit = pongRawClock.split(".")
                pongRaw = int(pongRawClockSplit[0]) * billion + int(pongRawClockSplit[1])

                recvRawClockSplit = recvRawClock.split(".")
                recvRaw = int(recvRawClockSplit[0]) * billion + int(recvRawClockSplit[1])

                pingRawClockSplit = pingRawClock.split(".")
                pingRaw = int(pingRawClockSplit[0]) * billion + int(pingRawClockSplit[1])

                #2018-09-16 00:12:16.028738088 UTC,losangeles-a,767220140697080,10,quebec-a,767170600445988
                #2018-09-16 00:12:16.1289813
                #zeroPad a direita!!!!!
                #timeAgrupator = recvDateTime_struct.strftime(formatoAgrupador)

                itemPingData = {
                    'receiverTime': receiverTime,
                    'receiverIp': receiverIp,
                    'recvRawClock': recvRawClock,
                    'pongTime':  pongTime,
                    'pongIp': pongIp,
                    'pongRawClock': pongRawClock,
                    'pingTime': pingTime,
                    'pingIp': pingIp,
                    'pingRawClock': pingRawClock,
                    'recvDateTime_struct': recvDateTime_struct
                }

                itemPingData['recvTimestamp'] = recvTimestamp
                itemPingData['pongTimestamp'] = pongTimestamp
                itemPingData['pingTimestamp'] = pingTimestamp

                itemPingData['recvRaw'] = recvRaw
                itemPingData['pongRaw'] = pongRaw
                itemPingData['pingRaw'] = pingRaw

                if pongServerName not in unidirectionalPingData:
                    unidirectionalPingData[pongServerName] = []

                unidirectionalPingData[pongServerName].append(itemPingData)

    return unidirectionalPingData

def findAllGzipedLogs(runDirectory, pingServerName):
    print(runDirectory + '*' + pingServerName + '*.log.gz')
    logFiles = glob.glob(runDirectory + '*' + pingServerName + '*.log.gz', recursive=False)
    return logFiles

def findAllLogsRecursive(runDirectory):
    logFiles = glob.glob(runDirectory + '**/*.log', recursive=True)
    return logFiles

for pingIp, pingServerName in cloudServers.items():

    logs = findAllGzipedLogs(runDirectory + '/data/', pingServerName)

    for log in logs:

        pprint(log)

        print("ExtractingPingData")
        print(str(datetime.now()))
        unidirectionalPingData = extractUnidirectionalPingData(log)
        print(str(datetime.now()))

        for pongServerName, data in unidirectionalPingData.items():

            recvDateTime_struct = data[0]['recvDateTime_struct']
            recvDateTime = recvDateTime_struct.strftime("%Y-%m-%d")
            logfileName = path_leaf(log)

            splitted = logfileName.split('-')
            dateTime = splitted[0] + '-' + splitted[1] + '-' + splitted[2]

            print("Writing pickle file")
            print(str(datetime.now()))

            path = runDirectory + '/pickle/unidirectional/' + pingServerName + '/' + pongServerName
            os.makedirs(path, exist_ok=True)
            picklefile = path + '/' + dateTime  + ".pickle"
            print("picklefile:" + picklefile)
            pickleOutput = open(picklefile, 'wb')
            pickle.dump(unidirectionalPingData[pongServerName], pickleOutput)
            print(str(datetime.now()))
