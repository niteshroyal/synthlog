import * as React from 'react';
import { FocusZone, FocusZoneDirection } from 'office-ui-fabric-react/lib/FocusZone';
import { TextField } from 'office-ui-fabric-react/lib/TextField';
import { List } from 'office-ui-fabric-react/lib/List';
import { Button, ButtonType } from 'office-ui-fabric-react';

export default class TableViewer extends React.Component {
    constructor(props, context) {
        super(props, context);
        this._originalItems = this.props.items;
    this.state = {
      filterText: '',
      items: this._originalItems
    };
    this.parent = this.props.parent;
    }

  render() { 
    const {
        parent,
        items2
    } = this.props;
    this.parent = parent;
    var items = this.state.items;
    const resultCountText = items.length === this._originalItems.length ? '' : ` (${items.length} of ${this._originalItems.length} shown)`;

    return (
        <div id="tables">
            <div>
            <h3>Tables</h3>
        <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.detectTables.bind(this)}>Detect tables</Button>
        </div>
      <FocusZone direction={FocusZoneDirection.vertical}>
        <TextField label={'Filter by name' + resultCountText} onChange={this._onFilterChanged} />
        <List items={items} onRenderCell={this._onRenderCell} />
      </FocusZone>
      <hr/>
      </div>
    );
  }

  appendItems(new_item) {
    const { items } = this.state;
  
    this.setState({
      items: items.concat([new_item])
    });
  }

  detectTables(){
    var that = this;
    var parameters = {file:Office.context.document.url};
    return fetch(`${this.parent.api}/detect_tables`, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(parameters)
    })
    .then(response => response.json())
    .then(function(json) {
      if (json.table_ranges){
          that.parent.state.tables = {};
          that.state.items = [];
        json.table_ranges.forEach(r => {
          that.parent.highlightRange(r,Object.keys(that.parent.state.tables).length);
          var default_key = "table" + Object.keys(that.parent.state.tables).length
          that.parent.state.tables[default_key] = r;
          that.appendItems(default_key);
        });
      }
      return json;
    })
    .catch(err => fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`))   
  }

  _onFilterChanged = (_, text)=> {
    this.setState({
      filterText: text,
      items: text ? this._originalItems.filter(item => item.toLowerCase().indexOf(text.toLowerCase()) >= 0) : this._originalItems
    });
  };

  _onRenderCell(item, index){
    var colors = ["#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b", "#eeca3b", "#b279a2", "#ff9da6", "#9d755d", "#bab0ac"];
    return (
          <div><p style={{ color: colors[index] }}>{item}</p></div>
    )
  }
}