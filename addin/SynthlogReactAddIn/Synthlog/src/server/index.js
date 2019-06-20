const express = require('express');
const bodyParser = require('body-parser');
// const pino = require('express-pino-logger')();
const cors = require('cors');
var structure = require('./librairies/structure');

const app = express();
app.use(cors());

app.use(bodyParser.urlencoded({ extended: false }));
// app.use(pino);

app.get('/api/init', (req, res) => {
    const inited = structure.init();
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify({ init: inited }));
});

app.get('/api/greeting', (req, res) => {
  const name = req.query.name || 'World';
  console.log(`Hello ${name}!`);
  res.setHeader('Content-Type', 'application/json');
  res.send(JSON.stringify({ greeting: `Hello ${name}` }));
});

// start the app
app.listen(3001, (error) => {
    if (error) {
        return console.log('something bad happened', error);
    }

    console.log("listening on 3000.");
});