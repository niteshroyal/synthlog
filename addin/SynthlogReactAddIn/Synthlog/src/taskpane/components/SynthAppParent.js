import * as React from 'react';
import 'isomorphic-fetch';
import ServerAPI from "./api";

const uuidv4 = require('uuid/v4');

export default class SynthAppParent extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.server_api = new ServerAPI();
    this.api = 'https://localhost:3001/api';
    this.objects_db = "";
    this.sqlite_db = "";

    this.sheet_ids = new Map(); // dictionary mapping sheet names to their id in the db
    this.colors = ["#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b", "#eeca3b", "#b279a2", "#ff9da6", "#9d755d", "#bab0ac"];

    this.currentState = { file: "", selection: "", tables: new Map() };
    this.db_is_loaded = false;

    this.state = {
      state_id: -1,
      tasks_suggestions: [],
      init_error: "",
      task_suggestions_enabled: true,
      loading: true,
      log: "",
      tables: [],
      blocks: [],
      constraints: [],
      tasks: [],
      activities: [],
      loading_tasks: true,
      predictions: [],
    };

    // this.addCurrentSheets();
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

  setStateAsync(newState) {
    console.log("Setting new state", newState);
      return new Promise(function(resolve, reject) {
        this.setState(newState, function() {
            resolve();
          });
      }.bind(this));
  }

  componentDidMount() {
    var that = this;
    this.initStructure();

  }
  assureDBIsLoaded() {
    if (!this.db_is_loaded) {
      this.db_is_loaded = true;
      return this.initSQLiteDB();
    }
  }

  initStructure() {
    fetch(`${this.api}/init_backend`)
      .catch(e => this.setState({init_error: e.toString()}))
        .then(() => this.initDatabases())
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

    if(this.state.task_suggestions_enabled) {
      return Excel.run(function (context) {
        return context.sync()
          .then(function () {
            that.currentState.selection = event.address;
            that.loadTaskSuggestions()
            // that.createState().then(that.getTaskSuggestions())
            // Call state change api and get new suggested actions
          });
      })
    }
  }

  loading() {

  }

  initState() {
    this.currentState.file = Office.context.document.url;
  }

  initDatabases() {
    this.initSQLiteDB();
  }

  initSQLiteDB() {
    const that = this;
    return this.server_api.setupSqlite()
        .then(json_res => this.sqlite_db = json_res.db_path)
      .then(() => this.addCurrentSheets(function() {
        this.server_api.getInitialState(Office.context.document.url)
            .then(that.loadState.bind(that));
      }.bind(this)))
  }

  loadState(state) {
    console.log("Loading state:", state);
    const activities = [];
    this.state.activities.forEach((v) => {
      if(v.previous_state_id < state.id) {
        activities.push(v)
      }
    });

      return this.setStateAsync({
        tables: state.tables,
        blocks: state.blocks,
        constraints: state.constraints,
        state_id: state.id,
        predictions: state.predictions,
        activities: activities,
        tasks: [],
        loading_tasks: true
      })
          .then(() => this.setStateAsync({loading: false}))
          .then(this.loadTaskSuggestions.bind(this));
  }

  loadStateFromId(state_id) {
    console.log("Load state with id:", state_id);
    return this.server_api.getState(state_id).then(this.loadState.bind(this));
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

  loadTaskSuggestions() {
    this.server_api.getTaskSuggestions(SynthAppParent.getDocumentUrl()).then((tasks) => {
      console.log("Tasks", tasks);
      if(tasks.exception) {
        console.log(tasks.exception);
      }
      const newState = {
        tasks: tasks.map((t) => {
          return {id: t.id, name: t.name}
        }),
        loading_tasks: false
      };
      return this.setStateAsync(newState);
    });
  }

  executeTask(task) {
    console.log("Execute task", task);
    const task_id = task.id;
    const activities = this.state.activities.slice();
    const self = this;
    this.server_api.executeTask(SynthAppParent.getDocumentUrl(), task_id)
        .then((newState) => {
          const activity_index = activities.length;
          const callback = function () {
            self.executeActivityAction(activity_index);
          };
          activities.push({
            name: task.name,
            previous_state_id: newState.previous_id,
            action: {
              name: "Undo",
              callback: callback,
            }
          });
          this.loadState(newState);
        }).then(() => {
      this.setStateAsync({activities: activities});
    });
  }

  executeActivityAction(activity_index) {
    const activity = this.state.activities[activity_index];
    const state_id = activity.previous_state_id;
    const activities = this.state.activities.filter((v, i) => i < activity_index);
    this.setStateAsync({activities: activities}).then(
        () => this.loadStateFromId(state_id)
    );
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
        that.setState({ tasks_suggestions: json.tasks });
        return json;
      })
      .catch(err => fetch(`${that.api}/log?type=${err.name}&message=${err.message}`))
  }

  performTask(task_id) {
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
        that.parseGuiActions(json.results);
        // Do something with the result
        return json;
      })
      .catch(err => fetch(`${that.api}/log?type=${err.name}&message=${err.message}`))
  }

  static getDocumentUrl() {
    return Office.context.document.url;
  }

  addCurrentSheets(callback=null) {
    var that = this;
    
    // Adds all sheets of the current workbook to the database
    // Ids are stored in this.sheet_ids

    Excel.run(async (context) => {
      var sheets = context.workbook.worksheets;
      sheets.load("items/name");
      var filename = Office.context.document.url;
      await context.sync().then(async function () {
        try {
          for (var i in sheets.items) {
            var parameters = { filename: filename, name: sheets.items[i].name, db_path: that.sqlite_db };
            await fetch(`${that.api}/add_sheet`, {
              method: 'POST',
              headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
              },
              body: JSON.stringify(parameters)
            }).then(response => response.json())
              .then(function (json) {
                that.sheet_ids.set(sheets.items[i].name, json.sheet_id);
              });
          }

        } catch (err) { fetch(`${that.api}/log?type=${err.name}&message=${err.message}`) };
      });

      if(callback) {
        callback();
      }
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
  };

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

  putCellValue(cell, value) {
    var that = this;

    Excel.run(function (context) {
      var sheet = context.workbook.worksheets.getActiveWorksheet();
      var range = sheet.getRange(cell);
      range.values = [[value]];
      range.format.autofitColumns();

      return context.sync();
    }).catch(function (err) {
      fetch(`${that.api}/log?type=${err.name}&message=${err.message}`);
    });
  }

  putRangeValue(input_range, value) {
    var that = this;
    Excel.run(function (context) {
      var sheet = context.workbook.worksheets.getActiveWorksheet();
      var range = sheet.getRange(input_range);
      range.values = [value];
      range.format.autofitColumns();

      return context.sync();
    }).catch(function (err) {
      fetch(`${that.api}/log?type=${err.name}&message=${err.message}`);
    });
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
  };

  getColors() {
    return this.colors;
  }

  parseGuiActions(result) {
    var regex_put_cell_value = /^put_cell_value (.+?) (.+)$/;
    var that = this;

    result.forEach(function (element) {
      var m = element.match(regex_put_cell_value);
      if (m) {
        that.putCellValue(m[1], m[2])
      }
    });
  }

}