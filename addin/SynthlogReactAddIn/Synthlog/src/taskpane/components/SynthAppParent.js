import * as React from 'react';
import 'isomorphic-fetch';
const uuidv4 = require('uuid/v4');

export default class SynthAppParent extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.api = 'https://localhost:3001/api';
    this.objects_db = "";
    this.sqlite_db = "";

    this.sheet_ids = new Map(); // dictionary mapping sheet names to their id in the db
    this.tables = new Map(); // list of active tables, either loaded through db or detected with python tool. Dictionary keys are table id, and value is the whole object
    this.nb_calls_add_table = 0;
    this.colors = ["#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b", "#eeca3b", "#b279a2", "#ff9da6", "#9d755d", "#bab0ac"]

    this.currentState = { file: "", selection: "", tables: new Map() };

    this.state = { state_id: -1, tasks_suggestions: [] , init_error:""}

    this.initSQLiteDB();
    this.addCurrentSheets();
    this.registerEventHandlers();
    this.initState();
  }

  render() {
    return (
      <div id='main' className='ms-welcome'>
        <div id="info">
        </div>
      </div>
    );
  }
  componentDidMount() {
    this.initStructure();
  }

  initStructure() {
    fetch(`${this.api}/init_backend`)
    .catch(e => this.setState({init_error: e.toString()}))
  }

  registerEventHandlers() {
    var that = this;

    try {
      Excel.run(function (context) {
        var sheet = context.workbook.worksheets.getActiveWorksheet();
        sheet.onChanged.add(that.sheetChangeHandler.bind(that));
        sheet.onSelectionChanged.add(that.sheetSelectionChangeHandler.bind(that));
        return context.sync()
          .then(function () {
            fetch(`${that.api}/log?type=ok&message=registered`)
          });
      })
    } catch (err) { fetch(`${that.api}/log?type=${err.name}&message=${err.message}`) }
  }

  sheetChangeHandler(event) {
    var that = this;
    fetch(`${that.api}/log?type=event&message=Changeevent!`)
    return Excel.run(function (context) {
      return context.sync()
        .then(function () {
          console.log("some event");
          // Ideally, triggers the state update for specific events
        });
    })
  }

  sheetSelectionChangeHandler(event) {
    var that = this;
    fetch(`${that.api}/log?type=Selectionevent&message=Changeevent!`);
    return Excel.run(function (context) {
      return context.sync()
        .then(function () {
          var selectedRange = event.address;
          that.currentState.selection = selectedRange
          that.createState().then(that.getTaskSuggestions())
          // Call state change api and get new suggested actions
        });
    })
  }

  initState() {
    this.currentState.file = Office.context.document.url;
  }

  initDatabases() {
    this.initSQLiteDB();
  }

  initSQLiteDB() {
    fetch(`${this.api}/init_sqlite_db`)
      .then(response => response.json())
      .then(json_res => this.sqlite_db = json_res.db_path);
  }

  createState() {
    var that = this;
    that.currentState.tables = that.tables;
    return fetch(`${this.api}/create_state`, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(that.currentState)
    })
      .then(response => response.json())
      .then(function (json) {
        that.setStateId(json.id);
        return json;
      })
      .catch(err => fetch(`${that.api}/log?type=${err.name}&message=${err.message}`))
  }

  getTaskSuggestions() {
    var that = this;
    var parameters = { state: that.state.state_id };
    //var parameters = {state: "latest"};
    return fetch(`${this.api}/get_tasks`, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(parameters)
    })
      .then(response => response.json())
      .then(function (json) {
        that.setState({ tasks_suggestions: json.tasks })
        return json;
      })
      .catch(err => fetch(`${that.api}/log?type=${err.name}&message=${err.message}`))
  }

  performTask(task_id){
    var that = this;
    var parameters = { state: that.state.state_id, task_id: task_id };
    //var parameters = {state: "latest"};
    return fetch(`${this.api}/execute_task`, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(parameters)
    })
      .then(response => response.json())
      .then(function (json) {
        // Do something with the result
        return json;
      })
      .catch(err => fetch(`${that.api}/log?type=${err.name}&message=${err.message}`))
  }

  addCurrentSheets() {
    var that = this;
    // Adds all sheets of the current workbook to the database
    // Ids are stored in this.sheet_ids
    Excel.run(async (context) => {
      var sheets = context.workbook.worksheets;
      sheets.load("items/name");
      var filename = Office.context.document.url
      await context.sync().then(function () {
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
              .then(function (json) {
                that.sheet_ids.set(sheets.items[i].name, json.sheet_id);
              })
          }
        } catch (err) { fetch(`${that.api}/log?type=${err.name}&message=${err.message}`) };
      });
    });
  }

  addTableFromRange = async (range) => {
    var that = this;
    var table_name = "table_" + uuidv4().slice(0, 6);
    var start = range.split(":")[0];
    var end = range.split(":")[1];
    var sheet_id = -1;

    await (this.getCurrentSheetId().then(s => sheet_id = s));

    var parameters = { db_path: that.sqlite_db, name: table_name, start_row: start.slice(1), start_col: start[0], end_row: end.slice(1), end_col: end[0], sheet_id: sheet_id };
    // TODO: separate letters and numbers properly, case with many letters as col is not handled yet
    return fetch(`${that.api}/add_table`, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(parameters)
    }).then(response => response.json())
      .then(function (json) {
        if (that.tables.has(json.table.id)) {
          return -1;
        }
        else {
          that.tables.set(json.table.id, json.table);
          return json.table.id;
        }
      })
  }

  getCurrentSheetId = async () => {
    var that = this;
    var sheet_id = -1;
    await Excel.run(async (context) => {
      var sheet = context.workbook.worksheets.getActiveWorksheet();
      sheet.load("name");

      await context.sync();
      sheet_id = that.sheet_ids.get(sheet.name);
    });
    return sheet_id;
  }

  getTables() {
    return this.tables;
  }

  getSheetIds() {
    return this.sheet_ids;
  }

  getSQLiteDB() {
    return this.sqlite_db;
  }

  getTableIndex(index) {
    return Array.from(this.tables.keys()).indexOf(index);
  }

  setStateId(id) {
    this.setState({ state_id: id });
  }

  getStateId() {
    return this.state.state_id;
  }

  highlightRange = async (range, table_id) => {
    var that = this;
    try {
      await Excel.run(function (context) {
        const sheets = context.workbook.worksheets;
        const firstSheet = sheets.getActiveWorksheet();
        var rangeExcel = firstSheet.getRange(range);
        var border_color = that.colors[table_id % that.colors.length];
        var border_style = "Continuous";
        var border_weight = "Thick";
        var border_list = ["EdgeRight", "EdgeLeft", "EdgeBottom", "EdgeTop"];
        border_list.forEach(b => {
          rangeExcel.format.borders.getItem(b).style = border_style;
          rangeExcel.format.borders.getItem(b).color = border_color;
          rangeExcel.format.borders.getItem(b).weight = border_weight;
        });
        return context.sync();
      })
        .catch(function (err) {
          fetch(`${that.api}/log?type=${err.name}&message=${err.message}`);
        })
    }
    catch (err) {
      fetch(`${this.api}/log?type=${err.name}&message=${err.message}`);
    }
  }

  getColors() {
    return this.colors;
  }

}