import * as React from 'react';
import { FocusZone, FocusZoneDirection } from 'office-ui-fabric-react/lib/FocusZone';
import { TextField } from 'office-ui-fabric-react/lib/TextField';
import { List } from 'office-ui-fabric-react/lib/List';
import { Button, ButtonType } from 'office-ui-fabric-react';
import SynthAppParent from "./SynthAppParent";

export default class TableViewer extends React.Component {
  constructor(props, context) {
    super(props, context);
    // this.parent = this.props.parent;
    // this._originalItems = Array.from(this.parent.getTables().keys());

    // this.refsTableField = [];

    this.state = {
      filterText: '',
      // items: this._originalItems.slice(),
    };
  }

  render() {
    // const {
    //   parent,
    // } = this.props;
    // this.parent = parent;
    // var items = this.state.items;
    // const resultCountText = items.length === this._originalItems.length ? '' : ` (${items.length} of ${this._originalItems.length} shown)`;
    //

    const tables = this.props.tables.map((item, index) => {
      return {
        name: item.name,
        color: this.props.colors[index]
      }
    });

    return (
      <div id="tables">
          <h3>Tables</h3>
        <FocusZone direction={FocusZoneDirection.vertical}>
          {/*<Button className='normal-button' buttonType={ButtonType.hero} onClick={this.detectTables.bind(this)}>Detect tables</Button>*/}
          {/*<TextField label={'Filter by name'} onChange={this._onFilterChanged.bind(this)} />*/}
          <List items={tables} onRenderCell={this._onRenderCell.bind(this)} />
        </FocusZone>
      </div>
    );
  }

  appendItems(new_item) {
    var items = this.state.items;
    if (this._originalItems.indexOf(new_item) < 0) {
      this.refsTableField.push(React.createRef());

      this._originalItems = this._originalItems.concat([new_item]);

      this.setState({
        items: items.concat([new_item])
      });

    }
  }

  loadTablesFromDB() {
    var that = this;

    for (const [sheet_name, sheet_id] of this.parent.getSheetIds()) {
      var parameters = { db_path: that.parent.getSQLiteDB(), id: sheet_id };
      fetch(`${that.parent.api}/get_sheet_tables`, {
        method: 'POST',
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(parameters)
      }).then(response => {
        return response.json();
      })
        .then(function (json) {
            that.props.loadTablesFn(json.tables)
        })
    }
  }

  detectTables = async () => {
    var that = this;
    var parameters = { file: Office.context.document.url };
    return fetch(`${this.parent.api}/detect_tables`, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(parameters)
    })
      .then(response => response.json())
      .then(async function (json) {
        if (json.table_ranges) {
            for(let i = 0; i < json.table_ranges.length; i++) {
                await that.parent.addTableFromRange(json.table_ranges[i]);
            }
            that.loadTablesFromDB();
          // json.table_ranges.forEach(async (r) => {
          //
          //   // try {
          //   //   if (default_key >= 0) {
          //   //     var index = that.parent.getTableIndex(default_key);
          //   //
          //   //     // that.parent.highlightRange(r, index);
          //   //     // that.appendItems(default_key);
          //   //   }
          //   // }
          //   // catch (err) {
          //   //   fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`)
          //   // }
          // });
        }
        return json;
      })
      .catch(err => fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`))
  };

  _onFilterChanged = (_, text) => {
    try {
      var that = this;
      var new_items = [];
      for (var i in this._originalItems) {
        if (this.parent.getTables().get(this._originalItems[i]).name.toLowerCase().indexOf(text.toLowerCase()) >= 0) {
          new_items.push(this._originalItems[i]);
        }
      }
      this.setState({
        filterText: text,
        items: text ? new_items : this._originalItems
      });
    } catch (err) {
      fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`);
    }
  };

  _onRenderCell(item, index) {
    const textStyle = {
      color: item.color,
      boxSizing: "border-box"
    };

    return (
      <TextField
          id={"tableName" + index}
          defaultValue={item.name}
          style={textStyle}
          onChange={this._onChange.bind(this)}
        onKeyPress={(e) => {
          if (e.key === 'Enter') {
            console.log('Enter key pressed');
            fetch(`${this.parent.api}/log?type=ok&message=enter pressed`);
            try {
              this.tableNameValidated(item, absolute_index);
            } catch (err) {
              fetch(`${this.parent.api}/log?type=${err.name}&message=${err.message}`);
            }
          }
        }} />
      //<div><p style={{ color: colors[index] }}>{name}</p></div>
    );

    // var colors = this.parent.getColors();
    // var name = this.parent.getTables().get(item).name;
    // var absolute_index = this._originalItems.indexOf(item);
    //
    // return (
    //   <TextField componentRef={this.refsTableField[absolute_index]} defaultValue={name} style={{ color: colors[absolute_index] }} onChange={this._onChange.bind(this)}
    //     onKeyPress={(e) => {
    //       if (e.key === 'Enter') {
    //         console.log('Enter key pressed');
    //         fetch(`${this.parent.api}/log?type=ok&message=enter pressed`);
    //         try {
    //           this.tableNameValidated(item, absolute_index);
    //         } catch (err) {
    //           fetch(`${this.parent.api}/log?type=${err.name}&message=${err.message}`);
    //         }
    //       }
    //     }} />
    //   //<div><p style={{ color: colors[index] }}>{name}</p></div>
    // )
  }

  _onChange(event, value) {
    try {
      fetch(`${this.parent.api}/log?type=text_change&message=${value}`);
    } catch (err) {
      fetch(`${this.parent.api}/log?type=${err.name}&message=${err.message}`);
    }
  }

  tableNameValidated = (item, index) => {
    var new_name = this.refsTableField[index].current.value;

    this.updateTableName(item, new_name);
  }

  updateTableName = (id, new_name) => {
    var table = this.parent.getTables().get(id);
    var that = this;
    var parameters = { db_path: this.parent.getSQLiteDB(), id: table.id, new_name: new_name };
    return fetch(`${this.parent.api}/update_table_name`, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(parameters)
    })
      .then(response => response.json())
      .then(json => {
        that.parent.getTables().set(json.table.id, json.table);
      })
  }

}