const express = require('express')
const app = express()

function getData(items) {
    const out = [];
    for(let i = 0; i < items; i++) {
    out.push({
        'id': i,
        'mac': "Some mac",
        'rssi': -1 * Math.random(),
        'battery': Math.random(),
        'uptime_ms': new Date().getTime(),
        'detection_timestamp': new Date().getTime()
    })
    }
    return out;
}

const data = getData(10000); 
const pageSize = 10;
const startTime = new Date().getTime();
const port = process.argv[2];

app.get('/detection/:id', (req, res) => {
    const id = parseInt(req.params.id);
    const maxItems = new Date().getTime() - startTime;
    const end = Math.min(maxItems, id + pageSize);
    const d = data.slice(id, end);
    res.setHeader("Content-Type", "application/json");
    res.end(JSON.stringify({"detections": d, "station_id": "station_"+port}))
})

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
