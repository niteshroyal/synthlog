const Os = require('os');
const Path = require('path');
const { PythonShell } = require('python-shell');
const homedir = Path.resolve(Os.homedir(), ".SynthLogBackEnd");


exports.getDefaultOptions = function(args) {
    const builtin_path = Path.resolve(homedir, 'resources');
    return {
        mode: 'text',
        scriptPath: builtin_path,
        pythonOptions: ['-u'],
        args: args,
        pythonPath: process.env.PYTHON_PATH,
    };
};

exports.runScriptDefault = function(script_name, args, res) {
    PythonShell.run(script_name, exports.getDefaultOptions(args), function (err, results) {
        if (err) {
            console.error(err.message);
            console.error(err.stack);
            res.setHeader('Content-Type', 'application/json');
            res.send({ error: err });
        }
        else {
            console.log("Initial state:", results[0]);
            res.setHeader('Content-Type', 'application/json');
            res.send(results[0]);
        }
    });
};
