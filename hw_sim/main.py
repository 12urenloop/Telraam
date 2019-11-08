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

def return_one_byte():
    return b'\x01'

batons = return_one_byte()

running = True
def get_id_baton():
    global batons
    batons += return_one_byte()
    return batons


beacons = return_one_byte()
def get_id_beacon():
    global beacons
    beacons += return_one_byte()
    return beacons

class Beacon:
    def __init__(self, host, port):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((host, port))
        self.id = get_id_beacon()

    def send(self, id):
        print("sending")
        bs = bytes(10)
        bs[0] = b'\x01'
        bs[1] = b'\x01'
        t = 345l
        print(1)
        bs[2] = t
        bs[3] = t << 8
        bs[4] = t << 16
        bs[5] = t << 24
        bs[6] = t << 32
        bs[7] = t << 40
        bs[8] = t << 48
        bs[9] = t << 56
        print(str(bs))
        self.socket.sendall(bs)
        # print(str(self.socket.send(bs)))
        # print(str(self.socket.send(id)))
        # self.socket.sendall(long(time.time() * 1000))

    def kill(self):
        self.socket.close()

class Runner(threading.Thread):
    def __init__(self, mean, dev, beacons):
        threading.Thread.__init__(self)
        self.mean = mean
        self.dev = dev
        self.beacons = itertools.cycle(beacons)
        self.id = get_id_baton()
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
        print("starting "+str(runner.id))

    try:
        while(True):
            time.sleep(1)
    except KeyboardInterrupt:
        quit(runners, beacons)
