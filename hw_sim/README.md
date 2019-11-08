# Hardware simulation script

## Requirements

Any* version of npm. Any* version of node.

## Setup

`npm install`

## Run

`node main.js`

## Arguments

\-h: Show all the help messages.
\-p: What port to connect to.
\-a: What address to connect to.
\-r: The amount of runners to spawn.
\-b: The amount of beacons to spawn.
\-m: The average time per round
\-d: Standard deviation for the average runner speed.
\-D: Standard deviation of round speed, global for all runners.

### Todo: added bonusses

- Loss rate.
- Malformed message rate. We need closing and starting tags to properly detect this. So server code and code here. Look at the class `telraam.beacon.BeaconMessage`.
- Messages per beacon or something, because irl the beacons are probably gonna send more than 1 message for a runner passing by.
