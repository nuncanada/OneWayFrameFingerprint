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

relations = [
    'pongRaw-pingRaw',
    'recvRaw-pongRaw',
    '2pongRaw-pingRaw-recvRaw',
    'pongTimestamp-pingTimestamp',
    'recvTimestamp-pongTimestamp',
    '2pongTimestamp-pingTimestamp-recvTimestamp'
]

def mapToPossibleInterestingRelations(unidirectionalPingData):
    possibleInterestingRelations = {}
    for item in unidirectionalPingData:
        timestamp = item['recvTimestamp']
        possibleInterestingRelations[timestamp] = {
            'pongRaw-pingRaw': item['pongRaw'] - item['pingRaw'],
            'recvRaw-pongRaw': item['recvRaw'] - item['pongRaw'],
            '2pongRaw-pingRaw-recvRaw': (2 * item['pongRaw'] - item['pingRaw'] - item['recvRaw']),
            'pongTimestamp-pingTimestamp': item['pongTimestamp'] - item['pingTimestamp'],
            'recvTimestamp-pongTimestamp': item['recvTimestamp'] - item['pongTimestamp'],
            '2pongTimestamp-pingTimestamp-recvTimestamp': (2 * item['pongTimestamp'] - item['pingTimestamp'] - item['recvTimestamp']),
        }

    return possibleInterestingRelations

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

def pickleFileToInterestingRelations(pickleFile):

    print("getUnidirectionalPingDataFromPickle")
    print("Começo: " + str(datetime.now()))
    unidirectionalPingData = pickle.load( open( pickleFile, "rb" ) )
    print("   Fim: " + str(datetime.now()))

    print("mapToPossibleInterestingRelations")
    print("Começo: " + str(datetime.now()))
    possibleInterestingRelations = mapToPossibleInterestingRelations(unidirectionalPingData)
    print("   Fim: " + str(datetime.now()))
    #pprint(pingDifference, file=currentLog)
    #pprint(rawClockCurrentTimeTranslation, file=currentLog)

    return possibleInterestingRelations

def plotData(htmlFile, normalizedData):
    htmlFilePath = os.path.dirname(htmlFile)
    os.makedirs(htmlFilePath, exist_ok=True)

    print("Plotting")
    print("Começo: " + str(datetime.now()))
    plotPing(normalizedData, htmlFile)
    print("   Fim: " + str(datetime.now()))

def limitDistantPoints(plotableData, percentageDiscarded):
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

    distanceFromLinearFitValues = []
    maxDiscarded = int(len(plotableData) * percentageDiscarded/100) + 1

    for x, y in plotableData.items():
        newY = (y - intercept) - (x * slope)
        queueValue = abs(newY)
        distanceFromLinearFitValues.append(queueValue)

    overTheLimit = sorted(distanceFromLinearFitValues, reverse=True)[:maxDiscarded]
    maxValue = overTheLimit[0]

    pprint(len(plotableData))
    pprint(maxDiscarded)
    pprint(maxValue)
    pprint(overTheLimit)
    pprint(len(overTheLimit))

    for x,y in plotableData.items():
        newY = (y - intercept) - (x * slope)
        if abs(newY) not in overTheLimit:
            result[x] = y
        else
            if newY > 0:
                sign = 1
            else
                sign = -1

            result[x] = maxValue * sign

    #pprint(lists, stream=currentLog)
    #pprint(result, stream=currentLog)

    return result

def limitDistantPointsAndLinearFit(plotableData, percentageDiscarded=10, formatoAgrupador="%Y-%m-%d %H:%M:%S"):

    plotableData = changeKeysFromDatetimeToTimestamp(plotableData, formatoAgrupador)

    if percentageDiscarded != 0:
        plotableData = limitDistantPoints(plotableData, percentageDiscarded)

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

def dataToPickleFile(filepath, data):
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
    normalizedData = limitDistantPointsAndLinearFit(plotableData, percentageDiscarded, formatoAgrupador);
    print("   Fim: " + str(datetime.now()))

    print("Plotting")
    print("Começo: " + str(datetime.now()))
    plotData(htmlFile, normalizedData)
    print("   Fim: " + str(datetime.now()))

def generatePickleAllDayInterestingRelations():
    rmtree(runDirectory + '/pickle/interestingRelations/', ignore_errors=True)

    for pingIp, pingServerName in cloudServers.items():
        for pongIp, pongServerName in cloudServers.items():

            if pingServerName <= pongServerName:
                pickleFiles = glob.glob(runDirectory + '/pickle/unidirectional/' + pingServerName + '/' + pongServerName + '/' + '*.pickle')
                unidirectionalInterestingRelationsAllDays = {}

                for pickleFile in pickleFiles:
                    print(pickleFile)
                    interestingRelations = pickleFileToInterestingRelations(pickleFile)
                    unidirectionalInterestingRelationsAllDays.update(interestingRelations)

                otherDirectionPickleFile = runDirectory + '/pickle/interestingRelations/' + pingServerName + '/' + pongServerName + '.pickle'
                dataToPickleFile(otherDirectionPickleFile, unidirectionalInterestingRelationsAllDays)

def generateHtmlInterestingRelations():
    rmtree(runDirectory + '/html/interestingRelations/', ignore_errors=True)

    for pingIp, pingServerName in cloudServers.items():
        for pongIp, pongServerName in cloudServers.items():

            if pingServerName <= pongServerName:
                pickleFile = runDirectory + '/pickle/interestingRelations/' + pingServerName + '/' + pongServerName + '.pickle'
                dataInterestingRelations = pickle.load( open( pickleFile, "rb" ) )

                for timestamp in dataInterestingRelations.keys():
                    for relation in relations:
                        htmlFile = runDirectory + '/html/interestingRelations/' + pingServerName + '/' + pongServerName + '_' + relation + '.pickle'

                if pingServerName < pongServerName:

                    otherDirectionPickleFile = runDirectory + '/pickle/interestingRelations/' + pongServerName + '/' + pingServerName + '.pickle'
                    dataOtherDirectionInterestingRelations = pickle.load( open( pickleFile, "rb" ) )




def wholePeriodUnidirectional():
    rmtree(runDirectory + '/html/whole_period/unidirectional/', ignore_errors=True)

    for pingIp, pingServerName in cloudServers.items():
        for pongIp, pongServerName in cloudServers.items():
            if pingServerName <= pongServerName:

                pickleFiles = glob.glob(runDirectory + '/pickle/unidirectional/' + pingServerName + '/' + pongServerName + '/' + '*.pickle')
                unidirectionalPingDataUnitingAllDays = {}

                for pickleFile in pickleFiles:
                    print(pickleFile)
                    plotableData = pickleFileToInterestingRelations(pickleFile, "%Y-%m-%d %H:%M")

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

    #rmtree(runDirectory + '/html/fft/unidirectional/', ignore_errors=True)

    for pingIp, pingServerName in cloudServers.items():
        for pongIp, pongServerName in cloudServers.items():
            if pingServerName < pongServerName:

                pickleFile = runDirectory + '/pickle/whole_period/unidirectional/' + pingServerName + '/' + pongServerName + '.pickle'
                plotableData = pickle.load( open( pickleFile, "rb" ) )

                pprint(plotableData)
                exit

                normalizedData = limitDistantPointsAndLinearFit(plotableData, 0, "%Y-%m-%d %H:%M")
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
