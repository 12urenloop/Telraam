#!/usr/bin/env python3

import itertools
import argparse
import time
import threading
import socket
import random
import signal
import sys


parser = argparse.ArgumentParser(description='Test script for telraam')

parser.add_argument('-p', "--port", action="store", default=4564, type=int, help="Port to use")
parser.add_argument('-a', "--address", action="store", default="127.0.0.1", help="Ip address to test")
parser.add_argument('-b', "--beacons", action="store", default=2, type=int, help="Amount of beacons")
parser.add_argument('-r', "--runners", action="store", default=5, type=int, help="Amount of runners")
parser.add_argument('-m', "--mean", action="store", default=500, type=int, help="Mean of runner speed (ms per round)")
parser.add_argument('-d', "--runner_deviation", action="store", default=10, type=int, help="Standard deviation of runner speed (per runner)")
parser.add_argument('-D', "--round_deviation", action="store", default=0, type=int, help="Standard deviation of runner speed (per round)")

def get_from_dist(mean, dev):
    return random.normalvariate(mean, dev)

id = 0

running = True
def get_id():
    global id
    id += 1
    return format(id, '010d')

class Beacon:
    def __init__(self, host, port):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((host, port))

    def send(self, id):
        self.socket.sendall(id)

    def kill(self):
        self.socket.close()

class Runner(threading.Thread):
    def __init__(self, mean, dev, beacons):
        threading.Thread.__init__(self)
        self.mean = mean
        self.dev = dev
        self.beacons = itertools.cycle(beacons)
        self.id = get_id()
        self.running = True

    def run(self):
        while(self.running):
            try:
                time.sleep(float(get_from_dist(self.mean, self.dev))/1000.0)
                next(self.beacons).send(str.encode(self.id))
            except Exception:
                pass

    def kill(self):
        self.running = False

def quit(runners, beacons):
    print("stopping")
    for runner in runners:
        runner.kill()

    for beacon in beacons:
        beacon.kill()

if __name__ == "__main__":

    configs = parser.parse_args()
    beacons = [Beacon(configs.address, configs.port) for x in range(configs.beacons)]

    runners = [Runner(get_from_dist(configs.mean, configs.runner_deviation), configs.round_deviation, beacons) for _ in range(configs.runners)]
    for runner in runners:
        runner.start()
        print("starting "+runner.id)

    try:
        while(True):
            time.sleep(1)
    except KeyboardInterrupt:
        quit(runners, beacons)
        sys.exit(0)
