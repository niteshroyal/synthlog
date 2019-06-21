import * as React from 'react';
import { Button, ButtonType } from 'office-ui-fabric-react';
import CheckLabel from './CheckLabel';
import Header from './Header';
import HeroList, { HeroListItem } from './HeroList';
import Progress from './Progress';
import 'isomorphic-fetch';

export default class App extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.api = 'https://localhost:3001/api';
    this.state = {
      init: false,
      init_error: '',
      python: false,
      idb: false
    };

    this.idb_running = false;
    this.handleClick = this.runIDBGeneration.bind(this);
  }

  componentDidMount() {
    this.initStructure();
    this.checkPython();
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
    else if (
      this.state.init && this.state.python && 
      !this.state.idb && !this.idb_running
    ) {
      this.idb_running = true;
      this.runIDBGeneration();
    }

    return (
      <div className='ms-welcome'>
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
        </div>
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

  initStructure() {
    fetch(`${this.api}/init`)
    .then(response => response.json())
    .then(json => this.setState(json))
    .catch(e => this.setState({init_error: e.toString()}))
  }

  runIDBGeneration = async() => {
    var that = this;
    try {
      await Excel.run(function(context) {
        const sheets = context.workbook.worksheets;
        const firstSheet = sheets.getActiveWorksheet();
        var range = firstSheet.getUsedRange();
        range.load(['rowIndex', 'columnIndex', 'values']);
        
        
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
                  values: range.values
                },
                homedir: {
                  idb: "synthlog.db"
                },
                script: "builtin/init.pl"
              })
            });
          })
          .then(function() {
            that.setState({idb: true});
          });
        }
      )
    }
    catch(err) {
      fetch(`${this.api}/log?type=${err.name}&message=${err.message}`);
    }
  }
}
