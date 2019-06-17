const AdmZip = require('adm-zip');
const FileSystem = require('fs');
const Request = require('request');
const Os = require('os');
const Path = require('path');

var homedir = Path.resolve(Os.homedir(), ".SynthLogBackEnd"); 
var inited = false;

function createDir(path) {
    if (!FileSystem.existsSync(path))
            FileSystem.mkdirSync(path);
}

function downloadProblog() {
    var path = Path.resolve(homedir, "master.tmp")
    var stream = Request('https://github.com/ML-KULeuven/problog/archive/master.zip')
        .pipe(FileSystem.createWriteStream(path));
    stream.on('finish',
            function(){
                var zip = new AdmZip(path);
                zip.extractAllToAsync(homedir, true);
                FileSystem.unlink(path, function(e){console.log(e)});
                FileSystem.renameSync(
                    Path.resolve(homedir, "problog-master"),
                    Path.resolve(homedir, "problog")
                );
                inited = true;
            }
        );
}

exports.init = function() {
    try {
        createDir(homedir);
        var problog_path = Path.resolve(homedir, "problog");
        if (!FileSystem.existsSync(problog_path))
            downloadProblog();

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

    }
    catch(error) {
        console.error(error);
        return false;
    }
    return true;
}

function importSynthlog() {

}
