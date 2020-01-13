import * as React from 'react';
import {FocusZone, FocusZoneDirection} from 'office-ui-fabric-react/lib/FocusZone';
import {List} from 'office-ui-fabric-react/lib/List';
import {Button, ButtonType, DefaultButton, Label} from "office-ui-fabric-react";

export default class ActivityLog extends React.Component {
    constructor(props, context) {
        super(props, context);
    }

    render() {
        console.log("Activities", this.props.activities);
        return (
            <div id="activity-log">
                <h3>Activity</h3>
                <FocusZone direction={FocusZoneDirection.vertical}>
                    <List items={this.props.activities} onRenderCell={this._onRenderCell.bind(this)}/>
                </FocusZone>
            </div>
        );
    }

    _onRenderCell(item, index) {
        return (
            <div className="activity" style={{display:'flex'}}>
                <Label>{item.name}</Label>
                <DefaultButton
                    onClick={function() {this.props.loadState(item.previous_state_id)}.bind(this)}
                >Undo</DefaultButton>
            </div>
        );
    }
}