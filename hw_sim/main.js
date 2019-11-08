'use strict';

const ArgumentParser = require('argparse').ArgumentParser;
const net = require('net');

var args;

// Standard Normal variate using Box-Muller transform.
function randn_bm(mean, dev) {
    var u = 0,
        v = 0;
    while (u === 0) u = Math.random(); //Converting [0,1) to (0,1)
    while (v === 0) v = Math.random();
    const out = Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
    return out * dev + mean;
}

class Beacon {
    constructor(id) {
        this.socket = net.Socket();
        this.socket.connect(args.port, args.address);
        this.id = id;
    }

    send(id) {
        const buffer = Buffer.alloc(10);
        console.log("Beacon", id, "Runner", this.id);
        buffer.writeInt8(this.id);
        buffer.writeInt8(id, 1);
        buffer.writeBigInt64LE(BigInt(Date.now()), 2);
        this.socket.write(buffer);
    }
}

class Runner {
    constructor(id, mean, beacons) {
        this.mean = mean;
        this.dev = args.round_deviation;
        this.beacons = beacons;
        this.at = 0;
        this.id = id;

        this.run = this.send.bind(this);

        this.set_next();
    }

    send() {
        this.beacons[this.at].send(this.id);
        this.at++;

        if (this.at >= this.beacons.length) {
            this.at = 0;
        }

        this.set_next();
    }

    set_next() {
        const time_till_next = randn_bm(this.mean, this.dev);
        setTimeout(this.run, time_till_next);
    }
}

function main() {
    const parser = new ArgumentParser({
        version: '0.0.1',
        addHelp: true,
        description: 'Argparse example'
    });
    parser.addArgument(['-p', "--port"], { defaultValue: 4564, type: "int", help: "Port to use" });

    parser.addArgument(['-a', "--address"], { defaultValue: "127.0.0.1", help: "Ip address to test" });
    parser.addArgument(['-b', "--beacons"], { defaultValue: 2, type: "int", help: "Amount of beacons" });
    parser.addArgument(['-r', "--runners"], { defaultValue: 5, type: "int", help: "Amount of runners" });
    parser.addArgument(['-m', "--mean"], { defaultValue: 500, type: "int", help: "Mean of runner speed (ms per round)" });
    parser.addArgument(['-d', "--runner_deviation"], { defaultValue: 10, type: "int", help: "Standard deviation of runner speed (per runner)" });
    parser.addArgument(['-D', "--round_deviation"], { defaultValue: 0, type: "int", help: "Standard deviation of runner speed (per round)" });

    args = parser.parseArgs();

    const beacons = [];
    for (let i = 0; i < args.beacons; i++) {
        beacons.push(new Beacon(i + 1));
    }

    const runners = [];
    for (let i = 0; i < args.runners; i++) {
        runners.push(new Runner(i + 1, randn_bm(args.mean, args.runner_deviation), beacons));
    }
}
main();