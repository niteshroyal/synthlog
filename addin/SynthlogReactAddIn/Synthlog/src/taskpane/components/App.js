import * as React from 'react';
import CheckLabel from './CheckLabel';
import Progress from './Progress';
import 'isomorphic-fetch';
import TheoryLoader from './TheoryLoader';
import { PredictionDiv } from './PredictionDiv';


export default class App extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.api = 'https://localhost:3001/api';
    this.state = {
      active: '',
      init: false,
      init_problog: false,
      init_error: '',
      python: false,
      idb: false,
      theories: [],
      debug: '',
      message:''
    };

    this.idb_running = false;
    this.problog_running = false;
    this.python_running = false;
    this.handleClick = this.runIDBGeneration.bind(this);
  }

  componentDidMount() {
    this.initStructure();
  }

  click = async () => {
    try {
      await Excel.run(async context => {
        /**
         * Insert your Excel code here
         */
        const range = context.workbook.getSelectedRange();

        // Read the range address
        range.load("address");

        // Update the fill color
        range.format.fill.color = "yellow";

        await context.sync();
        console.log(`The range address was ${range.address}.`);
      });
    } catch (error) {
      console.error(error);
    }
  }

  render() {
    const {
      title,
      isOfficeInitialized,
    } = this.props;

    if (!isOfficeInitialized) {
      return (
        <Progress
          title={title}
          logo='assets/logo-filled.png'
          message='Please sideload your addin to see app body.'
        />
      );
    }
    else if (!this.problog_running && this.state.init) {
      this.problog_running = true;
      this.initProblog();
    }
    else if (!this.python_running && this.state.problog) {
      this.python_running = true;
      this.checkPython();
    }
    else if (!this.idb_running && this.state.python) {
      this.idb_running = true;
      this.runIDBGeneration();
    }

    return (
      <div id='main' className='ms-welcome'>
        <div id="info">
          <CheckLabel 
            message="Python command" 
            boolean={ this.state.python } 
          />
          <CheckLabel 
            message="Synthlog initialization" 
            boolean={ this.state.init }
          />
          <CheckLabel
            message="Inductive database initialization"
            boolean={ this.state.idb }
          />
          <hr/>
        </div>
        
        <TheoryLoader
          active={ this.state.active }
          theories={ this.state.theories }
        />

        <div>
          <p>{ this.state.debug }</p>
        </div>

        <PredictionDiv parent={this}/>
      </div>
    );
  }

  /*
  Custom methods
  */
  checkPython() {
    fetch(`${this.api}/check_python`) 
    .then(response => response.json())
    .then(json => this.setState(json))
    .catch(e => this.setState({python: false}))
  }

  initProblog() {
    fetch(`${this.api}/init_problog`)
    .then(response => response.json())
    .then(json => this.setState({ problog: json.init }))
    .catch(e => this.setState({init_error: e.toString()}))
  }

  initStructure() {
    fetch(`${this.api}/init_backend`)
    .then(response => response.json())
    .then(json => this.setState(json))
    .catch(e => this.setState({init_error: e.toString()}))
  }

  loadTheories(theories, active=false) {
    try {
      if (theories.length > 0) {
        this.setState({
            theories: theories, 
            active: theories[0],
            debug: theories[0].label
          });
      }
    }
    catch(err) {
      this.setState({debug: err.message});
    }
  }

  runIDBGeneration = async() => {
    var that = this;
    try {
      await Excel.run(function(context) {
        const sheets = context.workbook.worksheets;
        const firstSheet = sheets.getActiveWorksheet();
        var range = firstSheet.getUsedRange();
        range.load(['rowIndex', 'columnIndex', 'values', 'valueTypes']);
        
        return context.sync()
          .then(function() {
            fetch(`${that.api}/run_synthlog`, {
              method: 'POST',
              headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
              },
              body: JSON.stringify({
                cells: {
                  firstRow: range.rowIndex,
                  firstColumn: range.columnIndex, 
                  values: range.values,
                  valueTypes: range.valueTypes
                },
                homedir: {
                  idb: "synthlog.db"
                },
                script: "builtin/init.pl"
              })
            })
            .then(function(response) {
              that.setState({idb: true});
              return response.json();
            })
            .then(function(json) {
              if (json.theories)
                that.loadTheories(json.theories, true);
            });
          });
        }
      )
    }
    catch(err) {
      fetch(`${this.api}/log?type=${err.name}&message=${err.message}`);
    }
  }
}