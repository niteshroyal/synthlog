const express = require('express');
const bodyParser = require('body-parser');
// const pino = require('express-pino-logger')();
const cors = require('cors');
var structure = require('./librairies/structure');
var { PythonShell } = require('python-shell');

const app = express();
app.use(cors());

app.use(bodyParser.urlencoded({ extended: false }));
// app.use(pino);

app.get('/api/init', (req, res) => {
    const inited = structure.init();
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify({ init: inited }));
});

app.get('/api/check_python', (req, res) => {
    PythonShell.runString('x=1;', null, function(err, results){
        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ python: !err }));
    });
});

app.get('/api/log', (req, res) => {
  const type = req.query.type || 'Log';
  console.log(`${type}: ${req.query.message}`);
  res.setHeader('Content-Type', 'application/json');
  res.send(JSON.stringify({ log: true }));
});

app.use(express.json());
app.post('/api/run_synthlog', (req, res) => {
    console.log("here!");
    console.log(req.body);
    if (req.body.script) {
        structure.generateParameters(req.body);
        structure.runScript(req.body.script, res);
    }
});

// start the app
app.listen(3001, (error) => {
    if (error) {
        return console.log('something bad happened', error);
    }

    console.log("listening on 3000.");
});