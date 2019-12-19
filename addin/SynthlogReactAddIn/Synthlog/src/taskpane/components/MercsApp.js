import * as React from 'react';
import 'isomorphic-fetch';
import SynthAppParent from './SynthAppParent';
import TableViewer from './TableViewer';
import TasksComponent from './TasksComponent';
import { FocusZone, FocusZoneDirection } from 'office-ui-fabric-react/lib/FocusZone';
import { Button, ButtonType } from 'office-ui-fabric-react';
import { List } from 'office-ui-fabric-react/lib/List';


export default class MercsApp extends SynthAppParent {
  constructor(props, context) {
    super(props, context);
  }


  render() {
    var items = this.state.tasks_suggestions;
    return (
      <div id='main' className='ms-welcome'>
        <div id="info">
          <p>
            Mercs
            </p>
          <TableViewer parent={this} />
          <p>State id: {this.state.state_id}</p>
          <FocusZone direction={FocusZoneDirection.vertical}>
            <h2>Available Tasks</h2>
            <List items={items} onRenderCell={this._onRenderCell.bind(this)} />
          </FocusZone>
        </div>
      </div>
    );
  }

  _onRenderCell(item, index) {
    var that = this;
    return (
      <Button className='normal-button' buttonType={ButtonType.hero} onClick={(e) => {
        this.performTask(item.id).bind(this);
      }}>{item.descr}</Button>
    )
  }
}