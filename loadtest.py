#!/usr/bin/env python3
import sys
import time
import subprocess

NUM_CLIENTS = 4
SPAWN_RATE = 600 #PER MINUTE

SERVER = "server.py"
CLIENT = "random_bot.py"
PYTHON_EXEC = sys.executable if sys.platform != 'win32' else "python.exe"

if __name__ == "__main__":
    if len(sys.argv) > 1:
        NUM_CLIENTS = int(sys.argv[1])

    print("Starting Server")
    LOG_SERVER = open("./logs/log_server.log", "w")
    SERVER_PROC = subprocess.Popen([PYTHON_EXEC, SERVER], stdout=LOG_SERVER)

    CLIENTS = []
    for c in range(NUM_CLIENTS):
        print("Starting Client", c)
        log_c = open("./logs/logC%d.log"%(c), "w")
        CLIENTS.append(subprocess.Popen([PYTHON_EXEC, CLIENT, str(c)], stdout=log_c))
        time.sleep(60./SPAWN_RATE)

    try:
        while list(filter(lambda p: p.poll() is None, CLIENTS)):
            time.sleep(5)
    except KeyboardInterrupt:
        for proc in CLIENTS:
            proc.terminate()

    SERVER_PROC.terminate()
