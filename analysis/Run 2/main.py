import csv
import time
import gzip

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

import pickle

formatoAgrupador = "%Y-%m-%d %H:%M:%S"
runDirectory = 'E:\\dados_experimento\\Run 2 - raw\\'

cloudServers = {
      '10.150.0.2': 'ashburn',
      '10.168.0.2': 'losangeles',
      '10.162.0.2': 'quebec',
      '10.158.0.2': 'saopaulo',
      '10.138.0.2': 'thedalles'
}

def plot_point_cov(points, nstd=2, ax=None, **kwargs):
    """
    Plots an `nstd` sigma ellipse based on the mean and covariance of a point
    "cloud" (points, an Nx2 array).

    Parameters
    ----------
        points : An Nx2 array of the data points.
        nstd : The radius of the ellipse in numbers of standard deviations.
            Defaults to 2 standard deviations.
        ax : The axis that the ellipse will be plotted on. Defaults to the
            current axis.
        Additional keyword arguments are pass on to the ellipse patch.

    Returns
    -------
        A matplotlib ellipse artist
    """
    pos = points.mean(axis=0)
    cov = np.cov(points, rowvar=False)
    return plot_cov_ellipse(cov, pos, nstd, ax, **kwargs)

def plot_cov_ellipse(cov, pos, nstd=2, ax=None, **kwargs):
    """
    Plots an `nstd` sigma error ellipse based on the specified covariance
    matrix (`cov`). Additional keyword arguments are passed on to the
    ellipse patch artist.

    Parameters
    ----------
        cov : The 2x2 covariance matrix to base the ellipse on
        pos : The location of the center of the ellipse. Expects a 2-element
            sequence of [x0, y0].
        nstd : The radius of the ellipse in numbers of standard deviations.
            Defaults to 2 standard deviations.
        ax : The axis that the ellipse will be plotted on. Defaults to the
            current axis.
        Additional keyword arguments are pass on to the ellipse patch.

    Returns
    -------
        A matplotlib ellipse artist
    """
    def eigsorted(cov):
        vals, vecs = np.linalg.eigh(cov)
        order = vals.argsort()[::-1]
        return vals[order], vecs[:,order]

    if ax is None:
        ax = plt.gca()

    vals, vecs = eigsorted(cov)
    theta = np.degrees(np.arctan2(*vecs[:,0][::-1]))

    # Width and height are "full" widths, not radius
    width, height = 2 * nstd * np.sqrt(vals)
    ellip = Ellipse(xy=pos, width=width, height=height, angle=theta, **kwargs)

    ax.add_artist(ellip)
    return ellip

def extractPingDifference(unidirectionalPingData):
    pingDifference = {}
    for agrupator in unidirectionalPingData:
        pingDifference[agrupator] = []
        for item in unidirectionalPingData[agrupator]:
            #sdiff = (2 * item['pongRaw'] + 2 * int(estimatedPongTimestampRawTranslation)) - item['pingRaw'] - item['recvRaw'] - 2*int(estimatedPingTimestampRawTranslation)
            sdiff = 2 * item['pongRaw'] - item['pingRaw'] - item['recvRaw']
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

def plotConfidenceEllipses(data):
    plot_point_cov(data, nstd=1, alpha=0.5, color='green')
    plot_point_cov(data, nstd=2, alpha=0.5, color='blue')
    plot_point_cov(data, nstd=3, alpha=0.5, color='red')

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

    plotly.offline.plot(data, filename=outputFileName)

def fromPingDataToNormalized(unidirectionalPingData):

    perMinuteStatistics = getMeanStdevData(pingDifference)
    print("perMinuteStatistics")
    print(str(datetime.now()))
    #pprint(perMinuteStatistics)
    #pprint(perMinuteStatistics, stream=currentLog)

    plotableData = useMeanAsValue(perMinuteStatistics)
    print("plotableData")
    print(str(datetime.now()))
    #pprint(plotableData)

    timestampKeyData = changeKeysFromDatetimeToTimestamp(plotableData)
    print("timestampKeyData")
    print(str(datetime.now()))
    #pprint(zeroStartingData)

    normalizedData = linearFitData(timestampKeyData)
    print("normalizedData")
    print(str(datetime.now()))
    #pprint(normalizedData)

    return normalizedData

def changeKeysFromDatetimeToTimestamp(diffPerMinute):
    listaTimestamp = {}

    for minuto, ping in diffPerMinute.items():
        listaTimestamp[datetime.strptime(minuto, formatoAgrupador).timestamp()] = ping

    return listaTimestamp


def changeKeysFromTimestampToDatetime(diffPerMinute):
    listaDatetime = {}

    for timestamp, ping in diffPerMinute.items():
        listaDatetime[datetime.utcfromtimestamp(timestamp).strftime(formatoAgrupador)] = ping

    return listaDatetime


def getUnidirectionalPingDataFromPickle(log, pongServerName):
    picklefile = log + "_" + pongServerName + "_unidirectional.pickle"
    unidirectionalPingData = pickle.load( open( picklefile, "rb" ) )
    return unidirectionalPingData

for pingIp, pingServerName in cloudServers.items():

    logs = findAllGzipedLogs(runDirectory + '/' + pingServerName + '/')

    for pongIp, pongServerName in cloudServers.items():
        unidirectionalPingDataUnitingAllDays = {}
        bidirectionalPingDataUnitingAllDays = {}

        for log in logs:

            pprint(log)
            currentLog = open(log + "_" + pongServerName + ".out", "w")

            #Apenas dados de um servidor por vez.
            print("getUnidirectionalPingDataFromPickle")
            print(str(datetime.now()))
            unidirectionalPingData = getUnidirectionalPingDataFromPickle(log, pongServerName)
            print(str(datetime.now()))
            #pprint(unidirectionalPingData)

            #Por enquanto pega o primeiro item mesmo... Não acho que valha a pena alguma coisa mais complicada
            #firstItem = unidirectionalPingData[list(unidirectionalPingData.keys())[0]][0]
            #pprint(firstItem)

            #estimatedPingTimestampRawTranslation = firstItem['pingTimestamp'] - firstItem['pingRaw']
            #estimatedPongTimestampRawTranslation = firstItem['pongTimestamp'] - firstItem['pongRaw']

            #pprint(unidirectionalPingData, file=currentLog)


            print("extractPingDifference")
            print(str(datetime.now()))
            pingDifference = extractPingDifference(unidirectionalPingData)
            print(str(datetime.now()))
            #pprint(pingDifference, file=currentLog)
            #pprint(rawClockCurrentTimeTranslation, file=currentLog)

            print("Normalizing Data")
            print(str(datetime.now()))
            normalizedData = fromPingDataToNormalized(unidirectionalPingData);
            print(str(datetime.now()))


            print("Changing from timestamp to DateTime")
            print(str(datetime.now()))
            dateTimeKeysData = changeKeysFromTimestampToDatetime(normalizedData)
            print(str(datetime.now()))

            print("Plotting")
            print(str(datetime.now()))
            plotPing(dateTimeKeysData, log + '_unidirectional_' + pongServerName + '.html')
            print(str(datetime.now()))

            #backwardsPerMinuteStatistics

            #diffPerMinute = subtractOneDirectionFromTheOther(perMinuteStatistics)
            #diffPerMinute = changeFromDateToMinuteStartingAtZero(diffPerMinute)
            #plotPingDifferences(diffPerMinute)

        unidirectionalPingDataUnitingAllDays = {**unidirectionalPingDataUnitingAllDays, **unidirectionalPingData}
        normalizedDataAllDays = fromPingDataToNormalized(unidirectionalPingDataUnitingAllDays);
        normalizedDataWithoutDeviantPointsAllDays = removePointsMaxDeviation(normalizedDataAllDays)
        dateTimeKeysDataAllDays = changeKeysFromTimestampToDatetime(normalizedDataWithoutDeviantPointsAllDays)
        plotPing(dateTimeKeysDataAllDays, runDirectory + 'unidirectional_' + pingServerName + '_' + pongServerName + '.html')

        #bidirectionalPingDataUnitingAllDays = {**bidirectionalPingDataUnitingAllDays, **bidirectionalPingData}
        #diffPerMinute = subtractOneDirectionFromTheOther(perMinuteStatistics)
        #pprint(diffPerMinute, file=currentLog)
