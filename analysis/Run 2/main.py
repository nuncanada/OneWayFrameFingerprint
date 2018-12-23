import csv
import time
from datetime import datetime
from pprint import pprint

import plotly
import plotly.plotly as py
import plotly.graph_objs as go
import plotly.io as pio

# MatPlotlib
import matplotlib.pyplot as plt
from matplotlib import pylab

# Scientific libraries
import numpy as np
from scipy import stats

import statistics

import sys
import glob

formatoAgrupador = "%Y-%m-%d %H:%M"
runDirectory = 'E:\\dados_experimento\\Run 2\\paths\\'

'''
[
    'E:/dados_experimento/Run 2/paths/10.162.0.2_10.138.0.2/2018-11-17.log',
    'E:/dados_experimento/Run 2/paths/10.162.0.2_10.138.0.2/2018-11-18.log',
    'E:/dados_experimento/Run 2/paths/10.162.0.2_10.138.0.2/2018-11-19.log',
    'E:/dados_experimento/Run 2/paths/10.168.0.2_10.162.0.2/2018-11-13.log',
    'E:/dados_experimento/Run 2/paths/10.168.0.2_10.162.0.2/sample.log',
    'E:/dados_experimento/Run 2/paths/10.168.0.2_10.158.0.2/2018-11-17.log',
]
'''

def extractUnidirectionalPingData(log):
    unidirectionalPingData={}
    rawClockCurrentTimeTranslation={}
    with open(log, newline='', encoding='utf-8') as f:
        reader = csv.reader(f)
        i=0
        for row in reader:
            i=i+1

            if len(row) > 9:
                receiverTime = row[0][5:]
                receiverIp = row[1]
                recvRawClock = row[2]
                pongTime = row[3][5:]
                pongIp = row[4]
                pongRawClock = row[5]
                pingTime = row[6][5:]
                pingIp = row[8]
                pingRawClock = row[9]

                #print('row: ', file=currentLog)
                #pprint(row, file=currentLog)

                #print('receiverTime' + receiverTime, file=currentLog)
                #print('receiverIp' + receiverIp, file=currentLog, file=currentLog)
                #print('pongTime' + pongTime, file=currentLog)
                #print('pongIp' + pongIp, file=currentLog)
                #print('pingTime' + pingTime, file=currentLog)
                #print('pingIp' + pingIp, file=currentLog)

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

                recvTimestamp = (int(time.mktime(recvDateTime_struct.timetuple())) * 1000000000) + int(recvNanoSeconds)
                pongTimestamp = (int(time.mktime(pongDateTime_struct.timetuple())) * 1000000000) + int(pongNanoSeconds)
                pingTimestamp = (int(time.mktime(pingDateTime_struct.timetuple())) * 1000000000) + int(pingNanoSeconds)

                pongRawClockSplit = pongRawClock.split(".")
                pongRaw = int(pongRawClockSplit[0]) * 1000000000 + int(pongRawClockSplit[1])

                recvRawClockSplit = recvRawClock.split(".")
                recvRaw = int(recvRawClockSplit[0]) * 1000000000 + int(recvRawClockSplit[1])

                pingRawClockSplit = pingRawClock.split(".")
                pingRaw = int(pingRawClockSplit[0]) * 1000000000 + int(pingRawClockSplit[1])

                #print('recvRaw: ' + str(recvRaw), file=currentLog)
                #print('pongRaw: ' + str(pongRaw), file=currentLog)
                #print('pingRaw: ' + str(pingRaw), file=currentLog)

                #print('recv: ' + str(time.mktime(recvDateTime_struct.timetuple())), file=currentLog)
                #print('pong: ' + str(time.mktime(pongDateTime_struct.timetuple())), file=currentLog)
                #print('ping: ' + str(time.mktime(pingDateTime_struct.timetuple())), file=currentLog)

                #print('pingTimestamp: ' + str(pingTimestamp), file=currentLog)
                #print('pongTimestamp: ' + str(pongTimestamp), file=currentLog)

                #print('recvNanoSeconds: ' + recvNanoSeconds, file=currentLog)
                #print('pongNanoSeconds: ' + pongNanoSeconds, file=currentLog)
                #print('pingNanoSeconds: ' + pingNanoSeconds, file=currentLog)

                #2018-09-16 00:12:16.028738088 UTC,losangeles-a,767220140697080,10,quebec-a,767170600445988
                #2018-09-16 00:12:16.1289813
                #zeroPad a direita!!!!!
                timeAgrupator = recvDateTime_struct.strftime(formatoAgrupador)

                if receiverIp not in unidirectionalPingData.keys():
                    unidirectionalPingData[receiverIp] = {}

                if pongIp not in unidirectionalPingData[receiverIp].keys():
                    unidirectionalPingData[receiverIp][pongIp] = {}

                if timeAgrupator not in unidirectionalPingData[receiverIp][pongIp].keys():
                    unidirectionalPingData[receiverIp][pongIp][timeAgrupator] = []

                itemPingData = {
                    'receiverTime': receiverTime,
                    'receiverIp': receiverIp,
                    'recvRawClock': recvRawClock,
                    'pongTime':  pongTime,
                    'pongIp': pongIp,
                    'pongRawClock': pongRawClock,
                    'pingTime': pingTime,
                    'pingIp': pingIp,
                    'pingRawClock': pingRawClock
                }

                itemPingData['recvTimestamp'] = recvTimestamp
                itemPingData['pongTimestamp'] = pongTimestamp
                itemPingData['pingTimestamp'] = pingTimestamp

                itemPingData['recvRaw'] = recvRaw
                itemPingData['pongRaw'] = pongRaw
                itemPingData['pingRaw'] = pingRaw

                if (not pingIp in rawClockCurrentTimeTranslation):
                    rawClockCurrentTimeTranslation[pingIp] = pingTimestamp - pingRaw

                if (not pongIp in rawClockCurrentTimeTranslation):
                    rawClockCurrentTimeTranslation[pongIp] = pongTimestamp - pongRaw

                unidirectionalPingData[receiverIp][pongIp][timeAgrupator].append(itemPingData)

        return unidirectionalPingData, rawClockCurrentTimeTranslation

def extractPingDifference(unidirectionalPingData, rawClockCurrentTimeTranslation):
    pingDifference = {}
    for receiver in unidirectionalPingData:
        pingDifference[receiver] = {}
        for sender in unidirectionalPingData[receiver]:
            pingDifference[receiver][sender] = {}
            for agrupator in unidirectionalPingData[receiver][sender]:
                pingDifference[receiver][sender][agrupator] = []
                for item in unidirectionalPingData[receiver][sender][agrupator]:
                    #pprint(rawClockCurrentTimeTranslation, file=currentLog)
                    #print('item[''pongRaw'']:' + str(item['pongRaw']), file=currentLog)
                    #print('item[''pingRaw'']:' + str(item['pingRaw']), file=currentLog)
                    #print('item[''recvRaw'']:' + str(item['recvRaw']), file=currentLog)
                    sdiff = (2 * item['pongRaw'] + 2 * int(rawClockCurrentTimeTranslation[sender])) - item['pingRaw'] - item['recvRaw'] - 2*int(rawClockCurrentTimeTranslation[receiver])
                    #print('sdiff: ' + str(sdiff), file=currentLog)
                    #sdiff = (2 * pongTimestamp) - pingTimestamp - recvTimestamp
                    pingDifference[receiver][sender][agrupator].append(sdiff)

    return pingDifference

def statiticalFitPingData(pingDifference):
    perMinuteStatistics = {}

    for senderName, dictReceivers in pingDifference.items():

        if senderName not in perMinuteStatistics.keys():
            perMinuteStatistics[senderName] = {}

        for receiverName, minutes in dictReceivers.items():

            if receiverName not in perMinuteStatistics[senderName].keys():
                perMinuteStatistics[senderName][receiverName] = {}

            for minute, oneWayLantecyList in minutes.items():

                if (len(oneWayLantecyList) > 1):
                    perMinuteStatistics[senderName][receiverName][minute] = {
                        'min':   min(oneWayLantecyList),
                        'mean':  statistics.mean(oneWayLantecyList),
                        'stdev': statistics.stdev(oneWayLantecyList)
                    }

    return perMinuteStatistics

def findAllLogs(runDirectory):
    logFiles = glob.glob(runDirectory + '**/*.log', recursive=True)
    return logFiles

#logs = findAllLogs(runDirectory)
logs = ['E:/dados_experimento/Run 2/paths/10.168.0.2_10.162.0.2/sample.log']
for log in logs:

    pprint(log)
    currentLog = open(log + ".out", "w")

    unidirectionalPingData, rawClockCurrentTimeTranslation = extractUnidirectionalPingData(log)
    #pprint(unidirectionalPingData, file=currentLog)

    pingDifference = extractPingDifference(unidirectionalPingData, rawClockCurrentTimeTranslation)
    #pprint(pingDifference, file=currentLog)
    #pprint(rawClockCurrentTimeTranslation, file=currentLog)

    perMinuteStatistics = statiticalFitPingData(pingDifference)
    pprint(perMinuteStatistics)
    pprint(perMinuteStatistics, stream=currentLog)

    diffPerMinute = {}

    for senderName, dictReceivers in perMinuteStatistics.items():
        for receiverName, minutes in dictReceivers.items():

            biggerIp = max(senderName, receiverName)
            smallerIp = min(senderName, receiverName)

            if biggerIp not in diffPerMinute.keys():
                diffPerMinute[biggerIp] = {}

            if smallerIp not in diffPerMinute[biggerIp].keys():
                diffPerMinute[biggerIp][smallerIp] = {}

            numOfEntries = 0
            minSum = 0
            meanSum = 0
            for minute, meanMinStdev in minutes.items():
                if (minute in perMinuteStatistics[receiverName][senderName]):
                    numOfEntries = numOfEntries + 1

                    if receiverName != senderName:
                        diffMean = perMinuteStatistics[senderName][receiverName][minute]['mean'] - perMinuteStatistics[receiverName][senderName][minute]['mean']
                        # 2*p(x) - p(x-1) - p(x+1) = 2*
                        #receiver - sender = 2*
                        diffMean = diffMean/4

                        #if abs(diffMean) > 300000:
                        #    diffMean = 300000 * (np.sign(diffMean))

                        dateSplit = minute.split(" ")
                        minuteSplit = dateSplit[1].split(":")
                        minutesSinceMidnight = int(minuteSplit[0]) * 60 + int(minuteSplit[1])

                        #pprint(minuteSplit, file=currentLog)
                        #print(minutesSinceMidnight, file=currentLog)

                        diffPerMinute[biggerIp][smallerIp][minute] = diffMean
                    else:
                        diffPerMinute[biggerIp][smallerIp][minute] = perMinuteStatistics[senderName][receiverName][minute]['mean']

            #pprint(diffPerMinute, file=currentLog)

    #pprint(diffPerMinute, file=currentLog)
    for key1 in diffPerMinute:
        for key2 in diffPerMinute[key1]:
            lists = sorted(diffPerMinute[key1][key2].items()) # sorted by key, return a list of tuples

            x, y = zip(*lists) # unpack a list of pairs into two tuples
            pprint(x)
            pprint(y)

            slope, intercept, r_value, p_value, std_err = stats.linregress(x,y)

            for x in diffPerMinute[key1][key2]:
                diffPerMinute[key1][key2][x] = diffPerMinute[key1][key2][x] - intercept - (x * slope)

            pprint(lists, stream=currentLog)

            pprint(diffPerMinute, stream=currentLog)

            elements = np.array(list(diffPerMinute[biggerIp][smallerIp].values()))
            #pprint(elements, file=currentLog)
            mean = np.mean(elements, axis=0)
            sd = np.std(elements, axis=0)

            print('mean: ' + str(mean), file=currentLog)
            print('sd: ' + str(sd), file=currentLog)

            maxDeviation = 2

            minValue = mean - maxDeviation * abs(sd)
            maxValue = mean + maxDeviation * abs(sd)

            for minutesSinceMidnight in diffPerMinute[biggerIp][smallerIp]:
                currentValue = diffPerMinute[biggerIp][smallerIp][minutesSinceMidnight]

                if currentValue > maxValue:
                    diffPerMinute[biggerIp][smallerIp][minutesSinceMidnight] = maxValue
                if currentValue < minValue:
                    diffPerMinute[biggerIp][smallerIp][minutesSinceMidnight] = minValue

            pprint(diffPerMinute, stream=currentLog)

            #pprint(x, file=currentLog)
            #pprint(y, file=currentLog)
            print('slope: ' + str(slope), file=currentLog)
            print('intercept: ' + str(intercept), file=currentLog)

            lists = sorted(diffPerMinute[key1][key2].items()) # sorted by key, return a list of tuples
            x, y = zip(*lists) # unpack a list of pairs into two tuples

            pprint(x, stream=currentLog)
            pprint(y, stream=currentLog)
            data = [go.Scatter(x=x,y=y)]
            plotly.offline.plot(data, filename=log + '.html')


#pprint(diffPerMinute, file=currentLog)


#        dadosQuebec = unidirectionalPingData['quebec-a']

#        diferencaEntreMedidas = {}
#        x = []
#        y = []
#        for key in dadosQuebec.keys():
#            if (key-1) in dadosQuebec.keys():
#                temp = dadosQuebec[key] - dadosQuebec[key-1]
#                x.append(key)
#                y.append(temp)
#                diferencaEntreMedidas[key] = temp

#        print("Direferenca entre medidas calculado", file=currentLog)

        # Generated linear fit
#        slope, intercept, r_value, p_value, std_err = stats.linregress(x,y)
        #line = slope*xi+intercept
#        print(slope, file=currentLog)
#        print(intercept, file=currentLog)
#        print(r_value, file=currentLog)
#        print(std_err, file=currentLog)
