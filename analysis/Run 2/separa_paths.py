import csv
import os
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

ips = [
    '10.138.0.2',
    '10.150.0.2',
    '10.158.0.2',
    '10.162.0.2',
    '10.168.0.2',
]

logs = [
    'owff_quebec-a.log',
    'owff_losangeles-a.log',
    'owff_saopaulo-a.log',
    'owff_thedalles-a.log',
    'owff_ashburn-a.log',
]

files = {}


for ip1 in ips:
    for ip2 in ips:
        biggerIp = max(ip1, ip2)
        smallerIp = min(ip1, ip2)
        newDir = 'E:/dados_experimento/Run 2/paths/'+ biggerIp + '_' + smallerIp
        if not os.path.exists(newDir):
            os.mkdir(newDir)

for log in logs:
    pprint(log)
    with open('E:/dados_experimento/Run 2/' + log, newline='', encoding='utf-8') as f:
        reader = csv.reader(f)
        i=0
        for row in reader:
            i=i+1

            if len(row) > 9:

                receiverTime = row[0][5:]
                receiverIp = row[1]
                pongTime = row[3][5:]
                pongIp = row[4]
                pingTime = row[6][5:]
                pingIp = row[8]

                recv = receiverTime.split(".")
                recvDateTime = recv[0]
                recvDate = recvDateTime.split(" ")[0]

                biggerIp = max(receiverIp, pongIp)
                smallerIp = min(receiverIp, pongIp)

                newDir = 'E:/dados_experimento/Run 2/paths/'+ biggerIp + '_' + smallerIp
                logFile = newDir + '/' + recvDate + '.log'

                if not logFile in files:
                    files[logFile] = open(logFile, 'x')

                files[logFile].write((','.join(row)) + "\n")

                if i%10000 == 0:
                    print('Linha ' + str(i))

for file in files:
    files[file].close()
