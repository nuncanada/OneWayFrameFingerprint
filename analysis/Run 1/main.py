import csv
import time
from datetime import datetime
from pprint import pprint

import plotly.plotly as py
import plotly.graph_objs as go

# MatPlotlib
import matplotlib.pyplot as plt
from matplotlib import pylab

# Scientific libraries
import numpy as np
from scipy import stats

import statistics


unidirectionalPingData = {
    'losangeles-a': {},
    'quebec-a': {},
    'ashburn-a': {},
    'thedalles-a': {},
    'saopaulo-a': {},
}

processarApenas = [
    'quebec-a',
    'thedalles-a'
]

formatoAgrupador = "%Y-%m-%d %H:%M"

logs = [
    #'owff_quebec_e_ethedalles_2018_09_30.log',
    #'owff_losangeles_2018_09_22_quebec.log',
    'owff_quebec-a_2018_09_22_losagenles.log'
]

for log in logs:
    with open('E:/dados_experimento/Run 1/' + log, newline='', encoding='utf-8') as f:
        reader = csv.reader(f)
        i=0
        for row in reader:
            i=i+1

            if len(row) > 6:
                receiverTime = row[0]
                receiverName = row[1]
                receiverClockCounter = row[2]
                senderMsgIndex = row[3]
                senderTime = row[4]
                senderName = row[5]
                senderClockCounter = row[6]

                rcc = receiverClockCounter.split(".")
                receiverCounter = (int(rcc[0]) * 1000000000) + int(rcc[1])

                scc = senderClockCounter.split(".")
                senderCounter = (int(scc[0]) * 1000000000) + int(scc[1])

                diffCounter = receiverCounter - senderCounter

                rt = receiverTime.split(".")
                st = senderTime.split(".")

                receiverDateTime = rt[0]
                senderDateTime = st[0]

                receiverSplitNanoSecondsTZ = rt[1].split(" ")
                senderSplitNanoSecondsTZ = st[1].split(" ")

                receiverNanoSeconds = receiverSplitNanoSecondsTZ[0] + ("0" * (9 - len(receiverSplitNanoSecondsTZ[0])))
                senderNanoSeconds = senderSplitNanoSecondsTZ[0] + ("0" * (9 - len(senderSplitNanoSecondsTZ[0])))

                formatoSemNano = "%Y-%m-%d %H:%M:%S"
                receiverDateTime_struct = datetime.strptime(receiverDateTime, formatoSemNano)
                senderDateTime_struct = datetime.strptime(senderDateTime, formatoSemNano)

                secondsDiff = int(time.mktime(receiverDateTime_struct.timetuple()) - time.mktime(senderDateTime_struct.timetuple()));
                nanoSecondsDiff = int(receiverNanoSeconds) - int(senderNanoSeconds);
                sdiff = (secondsDiff * 1000000000) + nanoSecondsDiff
                #2018-09-16 00:12:16.028738088 UTC,losangeles-a,767220140697080,10,quebec-a,767170600445988
                #2018-09-16 00:12:16.1289813
                #zeroPad a direita!!!!!

                timeAgrupator = receiverDateTime_struct.strftime(formatoAgrupador)

                if receiverName not in unidirectionalPingData[senderName].keys():
                    unidirectionalPingData[senderName][receiverName] = {}

                if timeAgrupator not in unidirectionalPingData[senderName][receiverName].keys():
                    unidirectionalPingData[senderName][receiverName][timeAgrupator] = []

                unidirectionalPingData[senderName][receiverName][timeAgrupator].append(sdiff)

perMinuteStatistics = {}

for senderName, dictReceivers in unidirectionalPingData.items():

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

#pprint(perMinuteStatistics)

diffPerMinute = {}

for senderName, dictReceivers in perMinuteStatistics.items():

    if senderName not in diffPerMinute.keys():
        diffPerMinute[senderName] = {}

    for receiverName, minutes in dictReceivers.items():
        if receiverName not in diffPerMinute[senderName].keys():
            diffPerMinute[senderName][receiverName] = {}

        numOfEntries = 0
        minSum = 0
        meanSum = 0
        for minute, meanMinStdev in minutes.items():

            if (minute in perMinuteStatistics[receiverName][senderName]):
                numOfEntries = numOfEntries + 1

                diffMean = perMinuteStatistics[senderName][receiverName][minute]['mean'] - perMinuteStatistics[receiverName][senderName][minute]['mean']

                if abs(diffMean) > 300000:
                    diffMean = 300000 * (np.sign(diffMean))

                diffPerMinute[senderName][receiverName][minute] = diffMean
                meanSum = meanSum + diffPerMinute[senderName][receiverName][minute]

        diffPerMinute[senderName][receiverName]['averageMean'] = meanSum/numOfEntries


pprint(diffPerMinute)

lists = sorted(diffPerMinute['thedalles-a']['quebec-a'].items()) # sorted by key, return a list of tuples

x, y = zip(*lists) # unpack a list of pairs into two tuples

plt.plot(x, y)
plt.show()

#pprint(diffPerMinute)


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

#        print("Direferenca entre medidas calculado")

        # Generated linear fit
#        slope, intercept, r_value, p_value, std_err = stats.linregress(x,y)
        #line = slope*xi+intercept
#        print(slope)
#        print(intercept)
#        print(r_value)
#        print(std_err)

def diffTimeWithNanoseconds(receiverTime, senderTime):
    rT = receiverTime.split(".")
    sT = senderTime.split(".")
