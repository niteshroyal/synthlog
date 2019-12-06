const FileSystem = require('fs');
const Os = require('os');
const sqlite3 = require('sqlite3');
const Path = require('path');

var homedir = Path.resolve(Os.homedir(), ".SynthLogBackEnd");

exports.init_sqlite_db = function (res) {
    var success = false;
    var db_path = Path.join(homedir, "sqlite_db.db")
    let db = new sqlite3.Database(db_path, (err) => {
        if (err) {
            console.error(err.message);
        }

        console.log('Connected to the sqlite database.');
        db.run("CREATE TABLE IF NOT EXISTS sheets (id INTEGER PRIMARY KEY AUTOINCREMENT, filename TEXT NOT NULL, name TEXT NOT NULL, UNIQUE(filename, name))");
        db.run("CREATE TABLE IF NOT EXISTS tables (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, start_row INT NOT NULL, start_col INT NOT NULL, nb_rows INT NOT NULL, nb_cols INT NOT NULL, sheet_id INT, FOREIGN KEY(sheet_id) REFERENCES sheets(id), UNIQUE(id,name))");
        success = true
    });
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify({ success: success, db_path: db_path }));
}

exports.add_sheet = function (params) {
    let db = new sqlite3.Database(params.db_path, (err) => {
        if (err) {
            console.error(err.message);
        }
        db.run(`INSERT OR IGNORE INTO sheets (filename, name) VALUES ("${params.filename}", "${params.name}")`);
    });
}

exports.get_sheet_id = function (params) {
    var id = -1;
    let db = new sqlite3.Database(params.db_path, (err) => {
        if (err) {
            console.error(err.message);
        }
        db.each(`SELECT id FROM sheets WHERE filename="${params.filename}" AND name="${params.name}"`, function(err, row){
            id = row.id
        })
    });
    return id;
}