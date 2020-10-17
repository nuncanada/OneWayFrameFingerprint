import csv
import time
import gzip

from datetime import datetime
from pprint import pprint

from dateutil.parser import parse

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
import os

import pickle

from shutil import rmtree

formatoAgrupadorPadrao = "%Y-%m-%d %H:%M:%S"
runDirectory = 'E:\\dados_experimento\\Run 3\\'

cloudServers = {
      '10.150.0.2': 'ashburn',
      '10.168.0.2': 'losangeles',
      '10.162.0.2': 'quebec',
      '10.158.0.2': 'saopaulo',
      '10.138.0.2': 'thedalles'
}

def extractPingDifference(unidirectionalPingData):
    pingDifference = {}
    for agrupator in unidirectionalPingData:
        pingDifference[agrupator] = []
        for item in unidirectionalPingData[agrupator]:
            #sdiff = (2 * item['pongRaw'] + 2 * int(estimatedPongTimestampRawTranslation)) - item['pingRaw'] - item['recvRaw'] - 2*int(estimatedPingTimestampRawTranslation)
            #sdiff = (2 * item['pongRaw'] - item['pingRaw'] - item['recvRaw']) / 2
            sdiff = item['pongRaw'] - item['pingRaw']
            pingDifference[agrupator].append(sdiff)

    return pingDifference

def getMeanStdevData(pingDifference):
    perMinuteStatistics = {}

    for minute, oneWayLantecyList in pingDifference.items():
        if (len(oneWayLantecyList) > 1):
            perMinuteStatistics[minute] = {
                'min':   min(oneWayLantecyList),
                'mean':  statistics.mean(oneWayLantecyList),
                'stdev': statistics.stdev(oneWayLantecyList)
            }

    return perMinuteStatistics

def useMeanAsValue(perMinuteStatistics):
    plotableData = {}
    for minute, statistics in perMinuteStatistics.items():
        plotableData[minute] = statistics['mean']

    return plotableData

def findAllLogsRecursive(runDirectory):
    logFiles = glob.glob(runDirectory + '**/*.log', recursive=True)
    return logFiles

def subtractOneDirectionFromTheOther(forwardPerMinuteStatistics, backwardsPerMinuteStatistics):

    diffPerMinute = {}

    numOfEntries = 0
    minSum = 0
    meanSum = 0
    for minute, meanMinStdev in forwardPerMinuteStatistics.items():
        if (minute in backwardsPerMinuteStatistics):
            numOfEntries = numOfEntries + 1

            diffMean = forwardPerMinuteStatistics[minute]['mean'] - backwardsPerMinuteStatistics[minute]['mean']
            # 2*p(x) - p(x-1) - p(x+1) = 2*
            #receiver - sender = 2*
            diffMean = diffMean/2

            #pprint(minuteSplit, file=currentLog)
            #print(minutesSinceMidnight, file=currentLog)

            diffPerMinute[minute] = diffMean

            #pprint(diffPerMinute, file=currentLog)

    return diffPerMinute

def linearFitData(pingDiffPerMinute, maxDeviation = 3):
    result = {}

    lists = sorted(pingDiffPerMinute.items())
    #print("lists")
    #pprint(lists)

    xTuple, yTuple = zip(*lists) # unpack a list of pairs into two tuples
    slope, intercept, r_value, p_value, std_err = stats.linregress(xTuple, yTuple)

    #pprint(xTuple, file=currentLog)
    #pprint(yTuple, file=currentLog)
    print('slope: ' + str(slope))
    print('intercept: ' + str(intercept))
    print('r_value: ' + str(r_value))
    print('p_value: ' + str(p_value))
    print('std_err: ' + str(std_err))

    minValue = - maxDeviation * abs(std_err)
    maxValue = maxDeviation * abs(std_err)

    for x, y in pingDiffPerMinute.items():
        newY = (y - intercept) - (x * slope)
        if (newY < minValue or newY > maxValue):
            result[x] = newY

    #pprint(lists, stream=currentLog)
    #pprint(result, stream=currentLog)

    return result

def removePointsMaxDeviation(pingDiffPerMinute, maxDeviation = 1):
    elements = np.array(list(pingDiffPerMinute.values()))
    #pprint(elements, file=currentLog)
    mean = np.mean(elements, axis=0)
    sd = np.std(elements, axis=0)

    print('mean: ' + str(mean), file=currentLog)
    print('sd: ' + str(sd), file=currentLog)

    minValue = mean - maxDeviation * abs(sd)
    maxValue = mean + maxDeviation * abs(sd)

    setWithPointsRemoved = {}
    for minutesSinceMidnight in pingDiffPerMinute:
        currentValue = pingDiffPerMinute[minutesSinceMidnight]

        #if (currentValue < maxValue and currentValue > minValue):
        setWithPointsRemoved[minutesSinceMidnight] = currentValue

    #    if currentValue > maxValue:
    #        pingDiffPerMinute[minutesSinceMidnight] = maxValue
    #    if currentValue < minValue:
    #        pingDiffPerMinute[minutesSinceMidnight] = minValue

    #pprint(setWithPointsRemoved, stream=currentLog)

    return setWithPointsRemoved

def plotPing(pingDiffPerMinute, outputFileName):

    lists = sorted(pingDiffPerMinute.items()) # sorted by key, return a list of tuples
    x, y = zip(*lists) # unpack a list of pairs into two tuples

    #pprint(x, stream=currentLog)
    #pprint(y, stream=currentLog)
    data = [go.Scatter(x=x,y=y)]

    plotly.offline.plot(data, filename=outputFileName, auto_open=False)

def fromPingDataToLinearFit(unidirectionalPingData):

    timestampKeyData = changeKeysFromDatetimeToTimestamp(unidirectionalPingData)
    print("timestampKeyData")
    print(str(datetime.now()))
    #pprint(zeroStartingData)

    normalizedData = linearFitData(timestampKeyData)
    print("normalizedData")
    print(str(datetime.now()))
    #pprint(normalizedData)

    print("Changing from timestamp to DateTime")
    print("Começo: " + str(datetime.now()))
    dateTimeKeysData = changeKeysFromTimestampToDatetime(normalizedData)
    print("   Fim: " + str(datetime.now()))

    return dateTimeKeysData

def changeKeysFromDatetimeToTimestamp(diffPerMinute, formatoAgrupador="%Y-%m-%d %H:%M:%S"):
    listaTimestamp = {}

    for minuto, ping in diffPerMinute.items():
        listaTimestamp[datetime.strptime(minuto, formatoAgrupador).timestamp()] = ping

    return listaTimestamp


def printSample(array, numberOfElementsToPrint=10):


def changeKeysFromTimestampToDatetime(diffPerMinute, formatoAgrupador="%Y-%m-%d %H:%M:%S"):
    listaDatetime = {}

    for timestamp, ping in diffPerMinute.items():
        listaDatetime[datetime.utcfromtimestamp(timestamp).strftime(formatoAgrupador)] = ping

    return listaDatetime

def groupByTimeFormat(unidirectionalPingData, formatoAgrupador):
    groupedData = {}
    currentGroupIndex = ""

    #2018-12-24 01:03:16.326274838 UTC

    for data in unidirectionalPingData:

        date = parse(data['pingTime'])
        formatoData = date.strftime(formatoAgrupador)
        #formatoData = formatoData[:-1] + "0"

        if formatoData != currentGroupIndex:
            groupedData[formatoData] = []

        groupedData[formatoData].append(data)
        currentGroupIndex = formatoData

    return groupedData

def pickleFileToPlotableData(pickleFile, formatoAgrupador="%Y-%m-%d %H:%M:%S"):

    print("getUnidirectionalPingDataFromPickle")
    print("Começo: " + str(datetime.now()))
    unidirectionalPingData = pickle.load( open( pickleFile, "rb" ) )
    print("   Fim: " + str(datetime.now()))

    print("groupedData")
    print("Começo: " + str(datetime.now()))
    groupedData = groupByTimeFormat(unidirectionalPingData, formatoAgrupador)
    print("   Fim: " + str(datetime.now()))
    #pprint(groupedData)

    print("extractPingDifference")
    print("Começo: " + str(datetime.now()))
    pingDifference = extractPingDifference(groupedData)
    print("   Fim: " + str(datetime.now()))
    #pprint(pingDifference, file=currentLog)
    #pprint(rawClockCurrentTimeTranslation, file=currentLog)

    perMinuteStatistics = getMeanStdevData(pingDifference)
    print("perMinuteStatistics")
    print(str(datetime.now()))
    #pprint(perMinuteStatistics)
    #pprint(perMinuteStatistics, stream=currentLog)

    plotableData = useMeanAsValue(perMinuteStatistics)
    print("plotableData")
    print(str(datetime.now()))
    #pprint(plotableData)

    return plotableData

def plotData(htmlFile, normalizedData):
    htmlFilePath = os.path.dirname(htmlFile)
    os.makedirs(htmlFilePath, exist_ok=True)

    print("Plotting")
    print("Começo: " + str(datetime.now()))
    plotPing(normalizedData, htmlFile)
    print("   Fim: " + str(datetime.now()))

def discardDistantPoints(plotableData, percentageDiscarded):
    result = {}
    lists = sorted(plotableData.items())
    #print("lists")
    #pprint(lists)

    xTuple, yTuple = zip(*lists) # unpack a list of pairs into two tuples
    slope, intercept, r_value, p_value, std_err = stats.linregress(xTuple, yTuple)

    #pprint(xTuple, file=currentLog)
    #pprint(yTuple, file=currentLog)
    print('slope: ' + str(slope))
    print('intercept: ' + str(intercept))
    print('r_value: ' + str(r_value))
    print('p_value: ' + str(p_value))
    print('std_err: ' + str(std_err))

    discardList = []
    distanceFromLinearFitValues = []
    maxDiscarded = int(len(plotableData) * percentageDiscarded/100)

    for x, y in plotableData.items():
        newY = (y - intercept) - (x * slope)
        queueValue = abs(newY)
        distanceFromLinearFitValues.append(queueValue)

    toBeDiscarded = sorted(distanceFromLinearFitValues, reverse=True)[:maxDiscarded]
    maxValue = toBeDiscarded[0]

    pprint(len(plotableData))
    print("maxDiscarded: " + maxDiscarded)
    print("maxValue: " + maxValue)
    pprint(len(toBeDiscarded))

    for x,y in plotableData.items():
        newY = (y - intercept) - (x * slope)
        if abs(newY) not in toBeDiscarded:
            result[x] = y
        else:
            result[x] = np.sign(newY) * maxValue

    #pprint(lists, stream=currentLog)
    #pprint(result, stream=currentLog)

    return result

def discardDistantPointsAndLinearFit(plotableData, percentageDiscarded=10, formatoAgrupador="%Y-%m-%d %H:%M:%S"):

    plotableData = changeKeysFromDatetimeToTimestamp(plotableData, formatoAgrupador)

    if percentageDiscarded != 0:
        plotableData = discardDistantPoints(plotableData, percentageDiscarded)

    result = {}
    lists = sorted(plotableData.items())
    #print("lists")
    #pprint(lists)

    xTuple, yTuple = zip(*lists) # unpack a list of pairs into two tuples
    slope, intercept, r_value, p_value, std_err = stats.linregress(xTuple, yTuple)

    #pprint(xTuple, file=currentLog)
    #pprint(yTuple, file=currentLog)
    print('slope: ' + str(slope))
    print('intercept: ' + str(intercept))
    print('r_value: ' + str(r_value))
    print('p_value: ' + str(p_value))
    print('std_err: ' + str(std_err))

    for x, y in plotableData.items():
        newY = (y - intercept) - (x * slope)
        result[x] = newY

    return changeKeysFromTimestampToDatetime(result, formatoAgrupador="%Y-%m-%d %H:%M:%S")

def plotableDataToPickleFile(filepath, data):
    print("plotableDataToPickleFile")
    print("Começo: " + str(datetime.now()))

    path = os.path.dirname(filepath)
    os.makedirs(path, exist_ok=True)
    pickleOutput = open(filepath, 'wb')
    pickle.dump(data, pickleOutput)
    print(str(datetime.now()))

    print("   Fim: " + str(datetime.now()))

def sumPingData(plotableData, plotableDataOtherDirection):
    summedList = {}

    for x, y in plotableData.items():
        if x in plotableDataOtherDirection.keys():
            summedList[x] = (plotableData[x] + plotableDataOtherDirection[x]) / 2

    return summedList

def normalizeAndPlot(htmlFile, plotableData, percentageDiscarded, formatoAgrupador):
    print("Normalizing Data")
    print("Começo: " + str(datetime.now()))
    normalizedData = discardDistantPointsAndLinearFit(plotableData, percentageDiscarded, formatoAgrupador);
    print("   Fim: " + str(datetime.now()))

    print("Plotting")
    print("Começo: " + str(datetime.now()))
    plotData(htmlFile, normalizedData)
    print("   Fim: " + str(datetime.now()))

def daily():
    rmtree(runDirectory + '/html/unidirectional/', ignore_errors=True)
    rmtree(runDirectory + '/html/bidirectional/', ignore_errors=True)

    for pingIp, pingServerName in cloudServers.items():
        for pongIp, pongServerName in cloudServers.items():

            pickleFiles = glob.glob(runDirectory + '/pickle/unidirectional/' + pingServerName + '/' + pongServerName + '/' + '*.pickle')

            unidirectionalPingDataUnitingAllDays = {}
            bidirectionalPingDataUnitingAllDays = {}

            i=0
            for pickleFile in pickleFiles:
                if pingServerName < pongServerName:

                    print(pickleFile)
                    logFile = pickleFile.replace("pickle", "log")
                    logFilePath = os.path.dirname(logFile)
                    os.makedirs(logFilePath, exist_ok=True)

                    currentLog = open(logFile, "w")

                    plotableData = pickleFileToPlotableData(pickleFile)

                    htmlFile = pickleFile.replace("pickle", "html")
                    normalizeAndPlot(htmlFile, plotableData, 3, formatoAgrupadorPadrao)

    #            if pingServerName < pongServerName:
                    pickleFileName = os.path.basename(pickleFile)
                    print(pickleFileName)
                    otherDirectionPickleFile = runDirectory + '/pickle/unidirectional/' + pongServerName + '/' + pingServerName + '/' + pickleFileName
                    plotableDataOtherDirection = pickleFileToPlotableData(otherDirectionPickleFile)

                    print("Other Direction!")
                    htmlFile = otherDirectionPickleFile.replace("pickle", "html")
                    normalizeAndPlot(htmlFile, plotableDataOtherDirection, 3, formatoAgrupadorPadrao)

                    bidirectionalData = sumPingData(plotableData, plotableDataOtherDirection)
                    bidirectionalPickleFileName = runDirectory + '/pickle/bidirectional/' + pingServerName + '/' + pongServerName + '/' + pickleFileName

                    plotableDataToPickleFile(bidirectionalPickleFileName, bidirectionalData)
                    htmlFile = bidirectionalPickleFileName.replace("pickle", "html")
                    normalizeAndPlot(htmlFile, bidirectionalData, 3, formatoAgrupadorPadrao)

                #backwardsPerMinuteStatistics

                #diffPerMinute = subtractOneDirectionFromTheOther(perMinuteStatistics)
                #diffPerMinute = changeFromDateToMinuteStartingAtZero(diffPerMinute)
                #plotPingDifferences(diffPerMinute)

            #unidirectionalPingDataUnitingAllDays = {**unidirectionalPingDataUnitingAllDays, **unidirectionalPingData}
            #normalizedDataAllDays = fromPingDataToNormalized(unidirectionalPingDataUnitingAllDays);
            #normalizedDataWithoutDeviantPointsAllDays = removePointsMaxDeviation(normalizedDataAllDays)
            #dateTimeKeysDataAllDays = changeKeysFromTimestampToDatetime(normalizedDataWithoutDeviantPointsAllDays)
            #plotPing(dateTimeKeysDataAllDays, runDirectory + 'unidirectional_' + pingServerName + '_' + pongServerName + '.html')

            #bidirectionalPingDataUnitingAllDays = {**bidirectionalPingDataUnitingAllDays, **bidirectionalPingData}
            #diffPerMinute = subtractOneDirectionFromTheOther(perMinuteStatistics)
            #pprint(diffPerMinute, file=currentLog)

def wholePeriodUnidirectional():
    rmtree(runDirectory + '/html/whole_period/unidirectional/', ignore_errors=True)

    for pingIp, pingServerName in cloudServers.items():
        for pongIp, pongServerName in cloudServers.items():
            if pingServerName <= pongServerName:

                pickleFiles = glob.glob(runDirectory + '/pickle/unidirectional/' + pingServerName + '/' + pongServerName + '/' + '*.pickle')
                unidirectionalPingDataUnitingAllDays = {}

                for pickleFile in pickleFiles:
                    print(pickleFile)
                    plotableData = pickleFileToPlotableData(pickleFile, "%Y-%m-%d %H:%M")

                    unidirectionalPingDataUnitingAllDays.update(plotableData)

                print("Writing pickle file")
                wholePeriodPickleFile = runDirectory + '/pickle/whole_period/unidirectional/' + pingServerName + '/' + pongServerName + '.pickle'
                plotableDataToPickleFile(wholePeriodPickleFile, unidirectionalPingDataUnitingAllDays)

                wholePeriodHtmlFile = wholePeriodPickleFile.replace('pickle', 'html')
                normalizeAndPlot(wholePeriodHtmlFile, unidirectionalPingDataUnitingAllDays, 2, "%Y-%m-%d %H:%M")

def regroupWithDateFormat(plotableData, oldDateFormat, newDateFormat):
    newPlotableData = {}
    for date in plotableData.keys():

        oldDate = datetime.strptime(date, oldDateFormat)
        newDate = oldDate.strftime(newDateFormat)

        if newDate not in newPlotableData:
            newPlotableData[newDate] = []

        newPlotableData[newDate].append(plotableData[date])

    averagedData = {}
    for date in newPlotableData.keys():
        averagedData[date] = np.sum(newPlotableData[date])/len(newPlotableData[date])

    return averagedData

def wholePeriodBidirectional():
    rmtree(runDirectory + '/html/whole_period/bidirectional/', ignore_errors=True)

    for pingIp, pingServerName in cloudServers.items():
        for pongIp, pongServerName in cloudServers.items():
            if pingServerName < pongServerName:

                pickleFiles = glob.glob(runDirectory + '/pickle/bidirectional/' + pingServerName + '/' + pongServerName + '/' + '*.pickle')
                bidirectionalPingDataUnitingAllDays = {}

                for pickleFile in pickleFiles:
                    print(pickleFile)
                    plotableData = pickle.load( open( pickleFile, "rb" ) )
                    plotableData = regroupWithDateFormat(plotableData, "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M")

                    bidirectionalPingDataUnitingAllDays.update(plotableData)

                print("Writing pickle file")
                wholePeriodPickleFile = runDirectory + '/pickle/whole_period/bidirectional/' + pingServerName + '/' + pongServerName + '.pickle'
                plotableDataToPickleFile(wholePeriodPickleFile, bidirectionalPingDataUnitingAllDays)

                wholePeriodHtmlFile = wholePeriodPickleFile.replace('pickle', 'html')
                normalizeAndPlot(wholePeriodHtmlFile, bidirectionalPingDataUnitingAllDays, 2, "%Y-%m-%d %H:%M")

def fftUnidirectional():

    np.random.seed(1234)

    time_step = 0.02
    period = 5.

    time_vec = np.arange(0, 20, time_step)
    sig = (np.sin(2 * np.pi / period * time_vec)
           + 0.5 * np.random.randn(time_vec.size))

    plt.figure(figsize=(6, 5))
    plt.plot(time_vec, sig, label='Original signal')

    #pprint(time_vec)
    #pprint(sig)

    rmtree(runDirectory + '/html/fft/unidirectional/', ignore_errors=True)

    for pingIp, pingServerName in cloudServers.items():
        for pongIp, pongServerName in cloudServers.items():
            if pingServerName < pongServerName:

                pickleFile = runDirectory + '/pickle/whole_period/unidirectional/' + pingServerName + '/' + pongServerName + '.pickle'
                plotableData = pickle.load( open( pickleFile, "rb" ) )

                normalizedData = discardDistantPointsAndLinearFit(plotableData, 0, "%Y-%m-%d %H:%M")
                #plotableDataTimestamp = changeKeysFromDatetimeToTimestamp(plotableData, "%Y-%m-%d %H:%M")

                pprint(normalizedData)
                splitedByWeek = {}
                lastWeek = ""
                for time in normalizedData.keys():
                    parsedTime = datetime.strptime(time, "%Y-%m-%d %H:%M:%S")
                    week = datetime.strftime(parsedTime, "%U")
                    if week != lastWeek:
                        splitedByWeek[week] = {}
                    splitedByWeek[week][time] = normalizedData[time]

                    lastWeek = week

                for week in splitedByWeek.keys():

                    zeroStartingData = []
                    for time in splitedByWeek[week].keys():
                        zeroStartingData.append(splitedByWeek[week][time])

                    lists = sorted(splitedByWeek[week].items()) # sorted by key, return a list of tuples
                    x, y = zip(*lists) # unpack a list of pairs into two tuples

                    #pprint(x, stream=currentLog)
                    #pprint(y, stream=currentLog)
                    data = [go.Scatter(x=x,y=y)]

                    plotly.offline.plot(data)

                    arrayForFft = []

                    for time in splitedByWeek[week].keys():
                        arrayForFft.append(abs(splitedByWeek[week][time]))

                    #pprint(arrayForFft)

                    Y = np.fft.fft(np.array(zeroStartingData))
                    freq = np.arange(0, len(zeroStartingData))

                    #data = [go.Scatter(x=x,y=y)]

                    print(pingServerName)
                    print(pongServerName)
                    pprint(Y)

                    pylab.figure()
                    pylab.plot( freq, np.abs(Y) )
                    pylab.figure()
                    pylab.plot(freq, np.angle(Y) )
                    pylab.show()
                    #plotly.offline.plot(fft)

def test():
    print("test")

def main():
    command = globals()[sys.argv[1]]
    command()

main()
