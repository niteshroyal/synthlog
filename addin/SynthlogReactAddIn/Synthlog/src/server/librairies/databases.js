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
    });

    console.log('Connected to the sqlite database.');
    db.run("CREATE TABLE IF NOT EXISTS sheets (id INTEGER PRIMARY KEY AUTOINCREMENT, filename TEXT NOT NULL, name TEXT NOT NULL, UNIQUE(filename, name))");
    db.run("CREATE TABLE IF NOT EXISTS tables (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, start_row INT NOT NULL, start_col TEXT NOT NULL, end_row INT NOT NULL, end_col TEXT NOT NULL, sheet_id INT, FOREIGN KEY(sheet_id) REFERENCES sheets(id), UNIQUE(sheet_id,name))");
    db.close()
    success = true

    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify({ success: success, db_path: db_path }));
}

exports.add_sheet = function (params) {
    let db = new sqlite3.Database(params.db_path, (err) => {
        if (err) {
            console.error(err.message);
        }
    });
    return new Promise(function (resolve, reject) {
        db.run(`INSERT OR IGNORE INTO sheets (filename, name) VALUES ("${params.filename}", "${params.name}")`, [], function (err) {
            if (err) {
                reject(err);
            }
            else {
                resolve(1);
            }
        });
        db.close();
    });
}

getProcessedFilename = function (filename) {
    return filename.replace(/\\\\/g, '\\');
}

exports.get_sheet_id = function (params, res) {
    let db = new sqlite3.Database(params.db_path, (err) => {
        if (err) {
            console.error(err.message);
        }
    });
    var filename_processed = getProcessedFilename(params.filename);
    console.log(filename_processed);
    db.get(`SELECT id FROM sheets WHERE filename="${filename_processed}" AND name="${params.name}"`, function (err, row) {
        if (row) {
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ sheet_id: row.id }));
        }
        else {
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ sheet_id: -1 }));
        }
    })
    db.close()
}

exports.get_table = function (params, res) {
    var final_obj = {};
    let db = new sqlite3.Database(params.db_path, (err) => {
        if (err) {
            console.error(err.message);
        }
    });
    if (params.hasOwnProperty('id')) {
        db.get(`SELECT * FROM tables WHERE id="${params.id}"`, function (err, row) {
            if (row) {
                res.setHeader('Content-Type', 'application/json');
                res.send(JSON.stringify({ table: row }));
            }
            else {
                res.setHeader('Content-Type', 'application/json');
                res.send(JSON.stringify({ table: {} }));
            }
        })
    }
    else {
        db.get(`SELECT * FROM tables WHERE sheet_id="${params.sheet_id}" AND name="${params.name}"`, function (err, row) {
            if (row) {
                res.setHeader('Content-Type', 'application/json');
                res.send(JSON.stringify({ table: row }));
            }
            else {
                res.setHeader('Content-Type', 'application/json');
                res.send(JSON.stringify({ table: {} }));
            }
        })
    }
    db.close()
}

exports.add_table = function (params) {
    let db = new sqlite3.Database(params.db_path, (err) => {
        if (err) {
            console.error(err.message);
        }
    });
    return new Promise(function (resolve, reject) {
        db.run(`INSERT OR IGNORE INTO tables (name, start_row, start_col, end_row, end_col, sheet_id) VALUES ("${params.name}","${params.start_row}","${params.start_col}","${params.end_row}","${params.end_col}","${params.sheet_id}")`, [], function (err) {
            if (err) {
                reject(err);
            }
            else {
                resolve(1);
            }
        });
        db.close();
    });

}

exports.update_table_name = function (params) {
    let db = new sqlite3.Database(params.db_path, (err) => {
        if (err) {
            console.error(err.message);
        }
    });
    return new Promise(function (resolve, reject) {
        db.run(`UPDATE tables SET name="${params.new_name}" WHERE id="${params.id}"`, [], function (err) {
            if (err) {
                reject(err);
            }
            else {
                resolve(1);
            }
        })
        db.close()
    });
}

exports.get_sheet_tables = function (params, res) {
    let db = new sqlite3.Database(params.db_path, (err) => {
        if (err) {
            console.error(err.message);
        }
    });
    db.all(`SELECT * FROM tables WHERE sheet_id="${params.id}"`, [], function (err, rows) {
        if (rows) {
            console.log("sheet_tables");
            console.log(rows)
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ tables: rows }));
        }
        else {
            res.setHeader('Content-Type', 'application/json');
            res.send(JSON.stringify({ tables: [] }));
        }
    })
    db.close();
}

