#!/usr/bin/env python3
import sys
import atexit
import time
import subprocess

NUM_CLIENTS = 4
SPAWN_RATE = 600 #PER MINUTE

SERVER = "server.py"
CLIENT = "random_bot.py"
PYTHON_EXEC = sys.executable if sys.platform != 'win32' else "python.exe"
SERVER_PROC = None
CLIENTS = []

def cleanup():
    """ Handler for test termination """
    if SERVER_PROC:
        SERVER_PROC.terminate()

    time.sleep(1)
    for proc in CLIENTS:
        proc.terminate()

if __name__ == "__main__":
    NO_SERVER_OPT = False

    if len(sys.argv) > 1:
        NUM_CLIENTS = int(sys.argv[1])
        if len(sys.argv) > 2:
            NO_SERVER_OPT = sys.argv[2] == "--noserver"

    if not NO_SERVER_OPT:
        print("Starting Server")
        LOG_SERVER = open("./logs/log_server.log", "w")
        SERVER_PROC = subprocess.Popen([PYTHON_EXEC, SERVER], stdout=LOG_SERVER)

    atexit.register(cleanup)
    try:
        for c in range(NUM_CLIENTS):
            print("Starting Client", c)
            log_c = open("./logs/logC%d.log"%(c), "w")
            CLIENTS.append(subprocess.Popen([PYTHON_EXEC, CLIENT, str(c)], stdout=log_c))
            time.sleep(60./SPAWN_RATE)

        while list(filter(lambda p: p.poll() is None, CLIENTS)):
            time.sleep(5)
    except KeyboardInterrupt:
        pass
