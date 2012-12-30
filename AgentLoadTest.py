import sys
import time
import subprocess

HALL_PROG = 'kingHallAgent.py'
CLIENT = 'kingBot.py'

NUM_CLIENTS = 2
EXEC = sys.executable if sys.platform != 'win32' else "python.exe"

SPAWN_RATE = 600 #PER MINUTE

#logHall = open("logHall.log","w")
#hall = subprocess.Popen([sys.executable, HALL_PROG], stdout=logHall)
print "Starting Hall Agent"
hall = subprocess.Popen([EXEC, HALL_PROG])

clients = []
for c in range(NUM_CLIENTS):
  #log_c = open("logC%d.log"%c,"w")
  print "Starting Client %s"%c
  clients.append(subprocess.Popen([EXEC, CLIENT, str(c)]))
  time.sleep(60./SPAWN_RATE)

try:
  while filter(lambda x: x.poll() == None, clients):
    time.sleep(5)  
except KeyboardInterrupt:
  for c in clients:
    c.terminate()

hall.terminate()
