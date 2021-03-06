const AdmZip = require('adm-zip');
const FileSystem = require('fs');
const Request = require('request');
const Os = require('os');
const Path = require('path');
const { PythonShell } = require('python-shell');
const Process = require('process');
var ncp = require('ncp').ncp;
require('dotenv').config();
const tools = require("./tools");

var homedir = Path.resolve(Os.homedir(), ".SynthLogBackEnd");
var inited = false;
var problog = false;

function createDir(path) {
    if (!FileSystem.existsSync(path))
        FileSystem.mkdirSync(path);
}

function downloadProblog(res) {
    var path = Path.resolve(homedir, "master.tmp")
    var stream = Request('https://github.com/ML-KULeuven/problog/archive/master.zip')
        .pipe(FileSystem.createWriteStream(path));
    stream.on('finish',
        function () {
            var zip = new AdmZip(path);
            zip.extractAllTo(homedir, true);
            FileSystem.unlink(path, function (e) { console.log(e) });
            FileSystem.renameSync(
                Path.resolve(homedir, "problog-master"),
                Path.resolve(homedir, "problog")
            );
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ init: true }));
        }
    );
}

function generateCells(cells) {
    var res = []

    if (cells) {
        var row_number = cells.values.length;
        var column_number = cells.values[0].length;

        for (var i = 0; i < row_number; i++) {
            for (var j = 0; j < column_number; j++) {
                res.push([
                    i + cells.firstRow + 1,
                    j + cells.firstColumn + 1,
                    cells.values[i][j],
                ])
            }
        }
    }

    return res;
}

function generateTypes(cells) {
    var res = []

    if (cells) {
        var row_number = cells.values.length;
        var column_number = cells.values[0].length;

        for (var i = 0; i < row_number; i++) {
            for (var j = 0; j < column_number; j++) {
                res.push(cells.valueTypes[i][j]);
            }
        }
    }

    return res;
}

exports.generateParameters = function (parameters) {
    const cells = generateCells(parameters.cells);
    const types = generateTypes(parameters.cells);
    let terms = generateTermStrings('cell', cells, false, types);
    for (var key in parameters) {
        if (!["cells", "script", "homedir"].includes(key)) {
            if (Array.isArray(parameters[key]))
                terms += generateTermStrings(key, parameters[key]);
            else
                terms += generateTermStrings(key, [[parameters[key]]]);
        }
        else if (key == "homedir") {
            for (var k in parameters["homedir"]) {
                if (Array.isArray(parameters[key]))
                    terms += generateTermStrings(
                        k,
                        parameters["homedir"][k],
                        true
                    );
                else
                    terms += generateTermStrings(
                        k,
                        [[parameters["homedir"][k]]],
                        true
                    );
            }
        }
    }
    // terms += "\nquery(excel:cell(1,1,_))."
    FileSystem.writeFileSync(Path.resolve(homedir, 'parameters.pl'), terms);
}

function generateTermStrings(key, values, fromhome = false, types = []) {
    let terms = "";
    values.forEach((element, i) => {
        args = "";
        var addTerm = true;
        element.forEach((arg, j) => {
            if (args != "") args += ",";
            a = arg.toString();
            if (fromhome) a = "'" + Path.resolve(homedir, a) + "'";
            // We add a quote if the argument is a string (except for the fist 2 arguments, that are x and y coordinates)
            if (types.length > 0) {
                if (types[i] == "String" && j > 1)
                    a = "'" + a + "'";
                // If it's an empty cell, we don't add it
                if (types[i] == "Empty")
                    addTerm = false;
            }
            args += a;
        });
        if (args != "") args = "(" + args + ")";
        if (addTerm) {
            // We consider that a data member is a term with a type
            var scope = types.length > 0 ? '(data)' : '(parameters)';
            terms += "excel" + scope + ":" + key + args + ". ";
        }
    });
    return terms;
}

function importSynthlog() {
    const path = Path.resolve(homedir, "synthlog");
    var zip = AdmZip(Path.resolve(__dirname, "..", "resources", "synthlog.zip"));
    zip.extractAllToAsync(path, true);
}

function init_builtin() {
    // Copies builtin files to home directory
    // Likely to be deprecated soon
    var builtin_path = Path.resolve(homedir, "builtin");
    createDir(builtin_path);

    var builtin_resource_path = Path.resolve(__dirname, "..", "resources", "builtin");
    var items = FileSystem.readdirSync(builtin_resource_path);

    items.forEach(function (item) {
        var resource_target_path = Path.resolve(builtin_path, item);
        if (!FileSystem.existsSync(resource_target_path)) {
            var resource_path = Path.resolve(builtin_resource_path, item);
            FileSystem.copyFileSync(resource_path, resource_target_path);
        }
    });

    // Copies all files from resources to the home directory
    var resources_path = Path.resolve(homedir, "resources");
    createDir(resources_path);

    builtin_resource_path = Path.resolve(__dirname, "..", "resources");
    // items = FileSystem.readdirSync(builtin_resource_path);

    ncp(builtin_resource_path, resources_path, function (err) {
        if (err) {
          return console.error(err);
        }
        console.log('Copied resources files!');
    });

    // items.forEach(function (item) {
    //     var resource_target_path = Path.resolve(resources_path, item);
    //     var resource_path = Path.resolve(builtin_resource_path, item);
    //     FileSystem.copyFileSync(resource_path, resource_target_path);
    // });
}

function columnToLetter(column) {
    var temp, letter = '';
    while (column > 0) {
        temp = (column - 1) % 26;
        letter = String.fromCharCode(temp + 65) + letter;
        column = (column - temp - 1) / 26;
    }
    return letter;
}

exports.init_problog = function (res) {
    var problog_path = Path.resolve(homedir, "problog");
    if (!FileSystem.existsSync(problog_path))
        downloadProblog(res);
    else {
        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ init: true }));
    }
}

exports.init = function (res) {
    try {
        createDir(homedir);

        var synthlog_path = Path.resolve(homedir, "synthlog");
        if (!FileSystem.existsSync(synthlog_path))
            importSynthlog();
        init_builtin();
        init_python();

        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ init: true }));
    }
    catch (error) {
        console.error(error);

        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ init: false }));
    }
};


function init_python(){
    // See for python env
    // nodeCmd.get('virtualenv ', (err, data, stderr) => console.log(data));
}

exports.getInitialState = function (filename, res) {
    console.log("Getting initial state for", filename);
    tools.runScriptDefault("state_api.py", ["initialize", filename], res);
};

exports.getState = function (state_id, res) {
    console.log("Getting state", state_id);
    tools.runScriptDefault("state_api.py", ["load", state_id], res);
};

exports.runScript = function (filename, res) {
    const problog_path = Path.resolve(homedir, 'problog');
    Process.chdir(Path.resolve(homedir, 'synthlog'));
    const options = {
        mode: 'text',
        scriptPath: problog_path,
        pythonOptions: ['-u'],
        args: [
            '--combine',
            Path.resolve(homedir, 'synthlog', 'synthlog', 'environment.pl'),
            Path.resolve(homedir, 'parameters.pl'),
            Path.resolve(homedir, filename)
        ]
    };
    PythonShell.run('problog-cli.py', options, function (err, results) {
        if (err) {
            console.error(err.message);
            console.error(err.stack);
            res.setHeader('Content-Type', 'application/json');
            res.send({ error: err });
        }
        else {
            console.log("Results: " + results);
            var active = null;
            var theories = new Set();
            var result_format = false;
            var result_output = [];
            results.forEach(element => {
                var splits = element.replace(/\s/g, '').split(':');
                if (splits.length > 2) {
                    if (!result_format && ['result', 'theory', 'active'].includes(splits[0])) {
                        result_format = true;
                        theories = new Set();
                    }
                    if (!result_format)
                        theories.add(splits[0]);
                    else {
                        console.log(splits)
                        if (splits[0] == 'theory') {
                            theories.add(splits[1]);
                            console.log('theory');
                        }
                        else if (splits[0] == 'active') {
                            active = splits[1];
                            console.log('active');
                        }
                        else if (splits[0] == 'result') {
                            result_output.push(splits[1]);
                            console.log('result');
                        }
                    }
                }
            });
            res.setHeader('Content-Type', 'application/json');
            console.log("Theories: " + Array.from(theories));
            var output = {
                output: result_format ? result_output : results,
                theories: Array.from(theories)
            };
            if (active != null)
                output.active = active;
            res.send(output);
        }
    });
}

exports.detect_tables = function (csv_file, res) {
    const builtin_path = Path.resolve(homedir, 'resources');
    const options = {
        mode: 'text',
        scriptPath: builtin_path,
        pythonOptions: ['-u'],
        args: [
            csv_file
        ],
        pythonPath: process.env.PYTHON_PATH,
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
                    const begin_row = parseInt(m[1]) + 1;
                    const end_row = parseInt(m[2]);
                    const begin_col = columnToLetter(parseInt(m[3]) + 1);
                    const end_col = columnToLetter(parseInt(m[4]));
                    result_output.push(begin_col + begin_row + ":" + end_col + end_row);
                }
            });
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ table_ranges: result_output }));
        }
    });
}

exports.extend_relevant = function (csv_file, relevant_ranges, unrelevant_ranges, tables, res) {
    // Call the actual python script for this
    console.log("In extending");
    console.log(tables);
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify({
        relevant_ranges: relevant_ranges, unrelevant_ranges: unrelevant_ranges,
        extended_relevant: ["magic_ice_cream!A6:B6"],
        extended_unrelevant: []
    }));
}