
const express = require('express')
const WebSocket = require('ws')
var WebSocketServer = require('ws').Server
const fs = require('fs');
const app = express()
const port = 8081

var wss = new WebSocketServer({ port: 3123 })

var data = {}
var inputFilesData = {}
loadInputFiles();

fs.watch('./../../../result_files/', function (event, filename) {
    if (event == 'change' || event == 'rename') {
        console.log('event is: ' + event);
        updateData();
        wss.clients.forEach(function each(client) {
            if (client.readyState === WebSocket.OPEN) {
                client.send(JSON.stringify({ data: data, inputFilesData: inputFilesData }));
            }
        });
    }

});

function loadInputFiles() {
    const files = fs.readdirSync('./../../../input_files/');
    var filesList = {}
    for (const file of files) {
        filesList[file] = fs.readFileSync('./../../../input_files/' + file, 'utf8');
    }

    for (const file in filesList) {
        if (file.indexOf('.txt') == -1) continue;
        var temp = filesList[file].split(/\r?\n/);
        var found = false;
        var tempList = []

        for (var i = 0; i < temp.length; i++) {
            if (temp[i] == '') {
                found = true
                continue;
            }
            if (found) {
                tempList.push(temp[i].split('\t'));
            }
        }
        inputFilesData[file.split('\.')[0]] = tempList;
    }
}

function updateData() {
    try {
        const files = fs.readdirSync('./../../../result_files/');
        data = {}
        for (const file of files) {
            if (file.indexOf('.txt') == -1) continue;
            data[file.split('\.')[0]] = fs.readFileSync('./../../../result_files/' + file, 'utf8');
        }
    } catch (err) {
        console.error("error reading files: ", err);
        console.error("retrying....");
        updateData();
    }
}

wss.on('connection', function (ws) {
    console.log('client connected');
    ws.send(JSON.stringify({ data: data, inputFilesData: inputFilesData }));
})


app.get('/', (req, res) => {
    res.sendFile('index.html', { root: __dirname })
})

app.listen(port, () => {
    console.log(`Example app listening on port ${port}`)
    updateData();
})