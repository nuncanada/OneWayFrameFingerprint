import csv
import time
import gzip

from datetime import datetime
from pprint import pprint

import sys
import glob

runDirectory = 'E:\\dados_experimento\\Run 2 - raw\\'

cloudServers = {
      '10.150.0.2': 'ashburn',
      '10.168.0.2': 'losangeles',
      '10.162.0.2': 'quebec',
      '10.158.0.2': 'saopaulo',
      '10.138.0.2': 'thedalles'
}

for serverIp, serverName in cloudServers.items():
    with open(runDirectory + serverName + '\\' + serverName + '.log', 'r') as f:
        reader = csv.reader(f)
        i=0
        ultimoDia = ""
        writer = ""
        for row in reader:
            i=i+1

            if len(row) > 9:
                receiverTime = row[0][5:]

                recv = receiverTime.split(".")

                recvDateTime = recv[0]

                recvSplitNanoSecondsTZ = recv[1].split(" ")

                recvNanoSeconds = recvSplitNanoSecondsTZ[0] + ("0" * (9 - len(recvSplitNanoSecondsTZ[0])))

                formatoSegundos = "%Y-%m-%d %H:%M:%S"
                recvDateTime_struct = datetime.strptime(recvDateTime, formatoSegundos)

                formatoDia = "%Y-%m-%d"
                diaAtual = recvDateTime_struct.strftime(formatoDia)

                if (ultimoDia != diaAtual):
                    ultimoDia = diaAtual
                    writer = csv.writer(open(runDirectory + serverName + '\\' + serverName + '_' + diaAtual + '.log', 'w'))

                writer.writerow(row)
