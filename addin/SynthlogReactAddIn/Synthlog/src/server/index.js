const express = require('express');
const bodyParser = require('body-parser');
const HttpsLocalhost = require('https-localhost');
// const pino = require('express-pino-logger')();
const cors = require('cors');
const Os = require('os');
var structure = require('./librairies/structure');
var databases = require('./librairies/databases');
var predictors = require('./librairies/predictors')
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
app.post('/api/add_sheet', async (req, res) => {
    console.log("adding a sheet to db");
    console.log(req.body);
    if (req.body) {
        await databases.add_sheet(req.body);
        databases.get_sheet_id(req.body, res);
    }
}
);

app.post('/api/call_mercs', async (req, res) => {
    console.log("calling mercs");
    console.log(req.body)
    if (req.body) {
        predictors.callMERCS(req.body, res);
    }
}
);

app.post('/api/create_state', async (req, res) => {
    console.log("Creating State");
    console.log(req.body)
    if (req.body) {
        predictors.createState(req.body, res);
    }
}
);

app.post('/api/initial_state', async (req, res) => {
    console.log("Getting initial state");
    console.log(req.body);
    if (req.body) {
        structure.getInitialState(req.body.filename, res);
    }
});

app.post('/api/get_state', async (req, res) => {
    console.log(req.body);
    if (req.body) {
        structure.getState(req.body.state_id, res);
    }
});

app.post('/api/get_tasks', async (req, res) => {
    console.log("Getting tasks suggestions");
    console.log(req.body);
    if (req.body) {
        predictors.getTasks(req.body, res);
    }
}
);

app.post('/api/execute_task', async (req, res) => {
    console.log("Executing task");
    console.log(req.body);
    if (req.body) {
        predictors.executeTask(req.body, res);
    }
}
);

app.post('/api/add_table', async (req, res) => {
    console.log("adding a table to db");
    console.log(req.body);
    if (req.body) {
        await databases.add_table(req.body);
        databases.get_table(req.body, res);
    }
}
);

app.post('/api/get_table', (req, res) => {
    if (req.body) {
        databases.get_table(req.body);
    }
}
);

app.post('/api/update_table_name', async (req, res) => {
    if (req.body) {
        await databases.update_table_name(req.body);
        databases.get_table(req.body, res);
    }
}
);

app.post('/api/get_sheet_tables', (req, res) => {
    console.log("getting tables")
    console.log(req.body);
    if (req.body) {
        databases.get_sheet_tables(req.body, res);
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