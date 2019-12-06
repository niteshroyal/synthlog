import * as React from 'react';
import 'isomorphic-fetch';

export default class SynthAppParent extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.api = 'https://localhost:3001/api';
    this.objects_db = "";
    this.sqlite_db = "";

    this.sheet_ids = {}; // dictionary mapping sheet names to their id in the db


    this.initSQLiteDB();
    this.addCurrentSheets();
  }


  render() {
    return (
      <div id='main' className='ms-welcome'>
        <div id="info">
        </div>
      </div>
    );
  }

  initDatabases() {
    this.initSQLiteDB();
  }

  initSQLiteDB() {
    fetch(`${this.api}/init_sqlite_db`)
      .then(response => response.json())
      .then(json_res => this.sqlite_db = json_res.db_path);
  }

  addCurrentSheets() {
    var that = this;
    // Adds all sheets of the current workbook to the database
    // Ids are stored in this.sheet_ids
    Excel.run(function (context) {
      var sheets = context.workbook.worksheets;
      sheets.load("items/name");
      var filename = Office.context.document.url
      return context.sync().then(function () {
        try {
          for (var i in sheets.items) {
            var parameters = { filename: filename, name: sheets.items[i].name, db_path: that.sqlite_db };
            fetch(`${that.api}/add_sheet`, {
              method: 'POST',
              headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
              },
              body: JSON.stringify(parameters)
            }).then(response => response.json())
            .then(function(json){
              that.sheet_ids[sheets.items[i].name] = response.sheet_id;
            })
          }
        } catch (err) { fetch(`${that.api}/log?type=${err.name}&message=${err.message}`) };
      });
    });
  }
}