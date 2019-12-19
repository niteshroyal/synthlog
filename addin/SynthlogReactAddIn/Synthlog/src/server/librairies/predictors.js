const AdmZip = require('adm-zip');
const FileSystem = require('fs');
const Request = require('request');
const Os = require('os');
const Path = require('path');
const { PythonShell } = require('python-shell');
const Process = require('process');

var homedir = Path.resolve(Os.homedir(), ".SynthLogBackEnd");

function createDir(path) {
    if (!FileSystem.existsSync(path))
        FileSystem.mkdirSync(path);
}

exports.createState = function (params, res) {
    const builtin_path = Path.resolve(homedir, 'resources');
    const options = {
        mode: 'text',
        scriptPath: builtin_path,
        pythonOptions: ['-u'],
        args: [
            "--create", "--filepath", params.file, "--selection", params.selection
        ],
        pythonPath: 'python',
    };

    PythonShell.run('state_manager.py', options, function (err, results) {
        if (err) {
            console.error(err.message);
            console.error(err.stack);
            res.setHeader('Content-Type', 'application/json');
            res.send({ error: err });
        }
        else {
            console.log("Results: " + results);
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ id: results[0] }));
        }
    });


}

exports.getTasks = function (params, res) {
    const builtin_path = Path.resolve(homedir, 'resources');
    var state = "latest"
    if (params.state >= 0) {
        state = params.state
    }
    const options = {
        mode: 'text',
        scriptPath: builtin_path,
        pythonOptions: ['-u'],
        args: [
            "--get", "--state", state
        ],
        pythonPath: 'python',
    };

    PythonShell.run('learner.py', options, function (err, results) {
        if (err) {
            console.error(err.message);
            console.error(err.stack);
            res.setHeader('Content-Type', 'application/json');
            res.send({ error: err });
        }
        else {
            console.log("Results: " + results);
            var result_output = [];
            var regex = /\(([0-9]+), '(.*)'\)/;
            results.forEach(element => {
                var m = element.match(regex);
                // Tacle indices are 0 based, excel is 1 based, hence the +1
                // But end indices seem to be 1 based in Tacle??
                if (m) {
                    result_output.push({ id: m[1], descr: m[2] });
                }
            });
            console.log("Tasks");
            console.log(result_output);
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ tasks: result_output }));
        }
    });
}

exports.executeTask = function (params, res) {
    const builtin_path = Path.resolve(homedir, 'resources');
    var state = "latest"
    if (params.state >= 0) {
        state = params.state
    }
    const options = {
        mode: 'text',
        scriptPath: builtin_path,
        pythonOptions: ['-u'],
        args: [
            "--execute", params.task_id, "--state", state
        ],
        pythonPath: 'python',
    };

    PythonShell.run('learner.py', options, function (err, results) {
        if (err) {
            console.error(err.message);
            console.error(err.stack);
            res.setHeader('Content-Type', 'application/json');
            res.send({ error: err });
        }
        else {
            console.log("Results: " + results);
            // var result_output = [];
            // var regex = /\(([0-9]+), '(.*)'\)/;
            // results.forEach(element => {
            //     var m = element.match(regex);
            //     // Tacle indices are 0 based, excel is 1 based, hence the +1
            //     // But end indices seem to be 1 based in Tacle??
            //     if (m) {
            //         result_output.push({id: m[1], descr:m[2]});
            //     }
            // });
            // console.log("Tasks");
            // console.log(result_output);
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ results: results }));
        }
    });
}

exports.callMERCS = function (params, res) {
    const builtin_path = Path.resolve(homedir, 'builtin');
    const options = {
        mode: 'text',
        scriptPath: builtin_path,
        pythonOptions: ['-u'],
        args: [
            csv_file
        ],
        pythonPath: 'python',
    };
    PythonShell.run('detect_tables.py', options, function (err, results) {
        if (err) {
            console.error(err.message);
            console.error(err.stack);
            res.setHeader('Content-Type', 'application/json');
            res.send({ error: err });
        }
        else {
            console.log("Results: " + results);
            var result_output = [];
            var regex = /\(([0-9]+):([0-9]+), ([0-9]+):([0-9]+)\)/;
            results.forEach(element => {
                var m = element.match(regex);
                // Tacle indices are 0 based, excel is 1 based, hence the +1
                // But end indices seem to be 1 based in Tacle??
                if (m) {
                    begin_row = parseInt(m[1]) + 1;
                    end_row = parseInt(m[2]);
                    begin_col = columnToLetter(parseInt(m[3]) + 1);
                    end_col = columnToLetter(parseInt(m[4]));
                    result_output.push(begin_col + begin_row + ":" + end_col + end_row);
                }
            });
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ table_ranges: result_output }));
        }
    });

    FileSystem.writeFileSync(Path.resolve(homedir, 'parameters.pl'), terms);
}
