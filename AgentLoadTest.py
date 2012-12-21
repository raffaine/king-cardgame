import sys
import time
import subprocess

HALL_PROG = 'kingHallAgent.py'
CLIENT = 'kingClient.py'

NUM_CLIENTS = 8

#logHall = open("logHall.log","w")
#hall = subprocess.Popen([sys.executable, HALL_PROG], stdout=logHall)

clients = []
for c in range(NUM_CLIENTS):
  log_c = open("logC%d.log"%c,"w")
  clients.append(subprocess.Popen([sys.executable,
                                   CLIENT, str(c)], stdout=log_c))

try:
  while filter(lambda x: not x.poll(), clients):
    time.sleep(10)
except KeyboardInterrupt:
  pass

for c in clients:
  c.terminate()
#hall.terminate()
