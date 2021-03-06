import * as React from 'react';
import 'isomorphic-fetch';
import ServerAPI from "./api";
import { BlockLayer, PredictionLayer, TableLayer, SelectionLayer } from "./Layers";

const uuidv4 = require('uuid/v4');

export default class SynthAppParent extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.server_api = new ServerAPI();
    this.api = 'https://localhost:3001/api';
    this.sqlite_db = "";

    this.sheet_ids = new Map(); // dictionary mapping sheet names to their id in the db
    // this.colors = ["#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b", "#eeca3b", "#b279a2", "#ff9da6", "#9d755d", "#bab0ac"];

    this.db_is_loaded = false;

    this.layers = [new BlockLayer(), new TableLayer(), new PredictionLayer(), new SelectionLayer()];
    var active_layers = this.layers.map(function (val, index) { return index });

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
      loading_tasks: false,
      predictions: [],
      active_layers: active_layers,
      ui_elements: [],
      graphic_context: {
        selection: "",
        formats: {},
        data_changes: {}
      }
    };

    // this.addCurrentSheets();
    this.registerEventHandlers();
  }

  render() {
    return (
      <div id='main' className='ms-welcome'>
        <div id="info">
        </div>
      </div>
    );
  }

  renderLayers() {
    let newUIElems = [];

    try {
      // Update state of all layers, as some of them might modify the state
      // Get UIElements on new state
      this.state.active_layers.forEach(layer_id => {
        const layer = this.layers[layer_id];
        newUIElems = newUIElems.concat(layer.getUIElements(this.state));
      });
      return this.setStateAsync({ ui_elements: newUIElems });
    } catch (err) {
      return this.server_api.log(err.name, err.message);
    }
  }

  setStateAsync(newState) {
    console.log("Setting new state", newState);
    return new Promise(function (resolve, reject) {
      this.setState(newState, function () {
        resolve();
      });
    }.bind(this));
  }

  componentDidMount() {
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
      .catch(e => this.setState({ init_error: e.toString() }))
      .then(() => this.initDatabases())
  }

  registerEventHandlers() {
    var that = this;

    try {
      Excel.run(function (context) {
        var sheet = context.workbook.worksheets.getActiveWorksheet();

        sheet.onChanged.add(that.sheetChangeHandler.bind(that));
        sheet.onSelectionChanged.add(that.sheetSelectionChangeHandler.bind(that));
        sheet.onFormatChanged.add(that.formatChangedHandler.bind(that));
        return context.sync()
          .then(function () {
            fetch(`${that.api}/log?type=ok&message=registered`)
          });
      })
    } catch (err) { fetch(`${that.api}/log?type=${err.name}&message=${err.message}`) }
  }

  sheetChangeHandler(event) {
    // changeType: Excel.DataChangeType | "Unknown" | "RangeEdited" | "RowInserted" | "RowDeleted" | "ColumnInserted" | "ColumnDeleted" | "CellInserted" | "CellDeleted";
    if (event.changeType == "RangeEdited") {
      Excel.run((context) => {
        try {
          context.workbook.save(Excel.SaveBehavior.Save); // available soon
        } catch (err) { this.server_api.log(err.name, err.message) }
        var modified_range = event.getRangeOrNullObject(context);
        modified_range.load(["values", "address"]);

        return context.sync().then(() => {
          try {
            let range_dict = this.state.graphic_context;
            range_dict.data_changes[modified_range.address] = modified_range.values;

            this.setState({ graphic_context: range_dict });
          } catch (err) { this.server_api.log(err.name, err.message) }
        })
      })
    }

    if (event.changeType == "RowDeleted") {
      // TODO: Update tables accordingly
    }
    if (event.changeType == "RowInserted") {
      // TODO: Update tables accordingly
    }
    if (event.changeType == "ColumnDeleted") {
      // TODO: Update tables accordingly
    }
    if (event.changeType == "ColumnInserted") {
      // TODO: Update tables accordingly
    }

    this.loadTaskSuggestions();
    // this.server_api.log("Address of event: ", event.address);
    // this.server_api.log("Source of event: ", event.source);
  }

  formatChangedHandler(event) {
    try {
      Excel.run((context) => {
        var modified_range = event.getRangeOrNullObject(context);
        modified_range.load(["format/fill/color", "format/font/bold", "format/font/color", "format/font/italic", "format/font/italic", "format/font/name", "format/font/size", "format/font/underline", "format/borders/*", "address"]);

        return context.sync().then(() => {
          try {
            let range_dict = this.state.graphic_context;
            range_dict["formats"][modified_range.address] = modified_range["format"];

            this.setStateAsync({ graphic_context: range_dict }).then(() => this.loadTaskSuggestions());
          } catch (err) { this.server_api.log(err.name, err.message) }
        })
      })
    } catch (err) { fetch(`${this.api}/log?type=${err.name}&message=${err.message}`) }
  }

  sheetSelectionChangeHandler(event) {
    try {
      const new_context = { ...this.state.graphic_context };
      new_context.selection = event.address;
      this.setStateAsync({
        graphic_context: new_context
      }).then(() => {
        this.loadTaskSuggestions();
      });
    } catch (err) { this.server_api.log(err.name, err.message) }
  }

  loading() {

  }

  initDatabases() {
    this.initSQLiteDB();
  }

  initSQLiteDB() {
    const that = this;
    return this.server_api.setupSqlite()
      .then(json_res => this.sqlite_db = json_res.db_path)
      .then(() => this.addCurrentSheets(function () {
        this.server_api.getInitialState(Office.context.document.url)
          .then(that.loadState.bind(that));
      }.bind(this)))
  }

  loadState(state) {
    console.log("Loading state:", state);
    const activities = [];
    this.state.activities.forEach((v) => {
      if (v.previous_state_id < state.id) {
        activities.push(v)
      }
    });

    const newState = this.augmentState({
      tables: state.tables,
      blocks: state.blocks,
      constraints: state.constraints,
      state_id: state.id,
      predictions: state.predictions,
      selections: state.selections,
      activities: activities,
      tasks: [],
      loading_tasks: false
    });

    return this.setStateAsync(newState)
      .then(() => this.setStateAsync({ loading: false }))
      .then(() => {
        this.renderLayers();
        this.loadTaskSuggestions();
      })
  }

  loadStateFromId(state_id) {
    console.log("Load state with id:", state_id);
    return this.server_api.getState(state_id).then(this.loadState.bind(this));
  }

  augmentState(json_state) {
    this.layers.forEach(layer => {
      json_state = layer.augmentState(json_state);
    });
    return json_state
  }

  setActiveLayers(active_layers) {
    this.setStateAsync({ active_layers: active_layers }).then(this.renderLayers.bind(this))
  }

  loadTaskSuggestions() {
    if (!this.state.loading_tasks) {
      this.setStateAsync({ loading_tasks: true }).then(() => {
        this.server_api.getTaskSuggestions(this.state.state_id, this.state.graphic_context).then((tasks) => {
          console.log("Tasks", tasks);
          if (tasks.exception) {
            console.log(tasks.exception);
          }
          const newState = {
            tasks: tasks.map((t) => {
              return { id: t.id, name: t.name }
            }),
            loading_tasks: false
          };
          return this.setStateAsync(newState);
        })
      });
    }
  }

  executeTask(task) {
    console.log("Execute task", task);
    const task_id = task.id;
    const activities = this.state.activities.slice();
    const self = this;
    this.server_api.executeTask(task_id)
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
        this.setStateAsync({ activities: activities });
      });
  }

  executeActivityAction(activity_index) {
    const activity = this.state.activities[activity_index];
    const state_id = activity.previous_state_id;
    const activities = this.state.activities.filter((v, i) => i < activity_index);
    this.setStateAsync({ activities: activities }).then(
      () => this.loadStateFromId(state_id)
    );
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

  addCurrentSheets(callback = null) {
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

      if (callback) {
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