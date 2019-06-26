const AdmZip = require('adm-zip');
const FileSystem = require('fs');
const Request = require('request');
const Os = require('os');
const Path = require('path');
const { PythonShell } = require('python-shell');
const Process =require('process');

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
            function(){
                var zip = new AdmZip(path);
                zip.extractAllTo(homedir, true);
                FileSystem.unlink(path, function(e){console.log(e)});
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

    if(cells) {
        var row_number = cells.values.length;
        var column_number = cells.values[0].length;

        for (var i = 0; i < row_number; i++) {
            for (var j = 0; j < column_number; j++) {
                res.push([
                    i+cells.firstRow, 
                    j+cells.firstColumn,
                    cells.values[i][j]
                ])
            }
        }
    }

    return res;
}

exports.generateParameters = function(parameters) {
    const cells = generateCells(parameters.cells);
    terms = generateTermStrings('cell', cells);
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
                    fromhome=true
                );
            else
                terms += generateTermStrings(
                    k, 
                    [[parameters["homedir"][k]]], 
                    fromhome=true
                );
            }
        }
    }
    FileSystem.writeFileSync(Path.resolve(homedir, 'parameters.pl'), terms);
}

function generateTermStrings(key, values, fromhome=false) {
    terms = "";
    values.forEach(element => {
        args = "";
        element.forEach(arg => { 
            if (args != "") args += ","; 
            a = arg.toString();
            if (fromhome) a = "'" + Path.resolve(homedir, a) + "'";
            args += a;
        });
        if (args != "") args = "(" + args + ")";
        terms += "excel:" + key + args + ". ";
    });
    return terms;
}

exports.init_problog = function(res) {
    var problog_path = Path.resolve(homedir, "problog");
    if (!FileSystem.existsSync(problog_path))
        downloadProblog(res);
    else {
        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ init: true }));
    }
}

exports.init = function(res) {
    try {
        createDir(homedir);

        var synthlog_path = Path.resolve(homedir, "synthlog");
        if (!FileSystem.existsSync(synthlog_path))
            importSynthlog();

        var builtin_path = Path.resolve(homedir, "builtin");
        createDir(builtin_path);

        var init_path = Path.resolve(builtin_path, "init.pl");
        if (!FileSystem.existsSync(init_path)) {
            resource_init = Path.resolve(__dirname, "..", "resources", "init.pl");
            FileSystem.copyFileSync(resource_init, init_path);
        }

        
        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ init: true }));
    }
    catch(error) {
        console.error(error);
        
        res.setHeader('Content-Type', 'application/json');
        res.send(JSON.stringify({ init: false }));
    }
}

function importSynthlog() {
    const path = Path.resolve(homedir, "synthlog");
    var zip = AdmZip(Path.resolve(__dirname, "..", "resources", "synthlog.zip"));
    zip.extractAllToAsync(path, true);
}

exports.runScript = function(filename, res) {
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
    PythonShell.run('problog-cli.py', options, function(err, results) {
        if (err) {
            console.error(err.message);
            console.error(err.stack);
            res.setHeader('Content-Type', 'application/json');
            res.send({error: err});
        }
        else {
            console.log(results);
            var theories = new Set();
            results.forEach(element => {
                var splits = element.split(':');
                if (splits.length > 2) theories.add(splits[0]);
            });
            res.setHeader('Content-Type', 'application/json');
            res.send({output: results, theories: theories});
        }
    });
}