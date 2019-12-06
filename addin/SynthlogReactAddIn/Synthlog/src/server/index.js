const express = require('express');
const bodyParser = require('body-parser');
const HttpsLocalhost = require('https-localhost');
// const pino = require('express-pino-logger')();
const cors = require('cors');
const Os = require('os');
var structure = require('./librairies/structure');
var databases = require('./librairies/databases');
var { PythonShell } = require('python-shell');
const Path = require('path');

const app = HttpsLocalhost();
app.use(cors());

app.use(bodyParser.urlencoded({ extended: false }));
// app.use(pino);

app.get('/api/init_backend', (req, res) => {
    console.log("init!")
    structure.init(res);
});

app.get('/api/init_problog', (req, res) => {
    structure.init_problog(res);
});

app.get('/api/check_python', (req, res) => {
    PythonShell.runString('x=1;', null, function (err, results) {
        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ python: !err }));
    });
});

app.get('/api/init_sqlite_db', (req, res) => {
    // Returns JSON object with 2 keys: success a boolean with ttrue value if init is successful; db_path the path to the created database
    console.log("init sqlite db")
    databases.init_sqlite_db(res);
});

app.get('/api/log', (req, res) => {
    const type = req.query.type || 'Log';
    console.log(`${type}: ${req.query.message}`);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify({ log: true }));
});

app.use(express.json());
app.post('/api/add_sheet', (req, res) => {
    console.log("adding a table to db");
    if (req.body) {
        databases.add_sheet(req.body);
        var id = databases.get_sheet_id(req.body);
        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ sheet_id: id }));
    }
}
);

app.post('/api/run_synthlog', (req, res) => {
    console.log("here!");
    console.log(req.body);
    if (req.body.script) {
        structure.generateParameters(req.body);
        structure.runScript(req.body.script, res);
    }
});

app.post('/api/detect_tables', (req, res) => {
    console.log("here2!");
    console.log(req.body);
    if (req.body.file) {
        structure.detect_tables(req.body.file, res);
    }
});

app.post('/api/extend_relevant', (req, res) => {
    console.log("here relevant!");
    console.log(req.body);
    if (req.body.file) {
        structure.extend_relevant(req.body.file, req.body.relevant_ranges, req.body.unrelevant_ranges, req.body.tables, res);
    }
});

// start the app
app.listen(3001, (error) => {
    if (error) {
        return console.log('something bad happened', error);
    }

    console.log("listening on 3001.");
});