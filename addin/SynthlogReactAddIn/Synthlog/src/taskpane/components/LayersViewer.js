import * as React from 'react';
import { FocusZone, FocusZoneDirection } from 'office-ui-fabric-react/lib/FocusZone';
import { List } from 'office-ui-fabric-react/lib/List';
import { Toggle } from "office-ui-fabric-react";

export default class LayersViewer extends React.Component {
    constructor(props, context) {
        super(props, context);
    }

    render() {
        return (
            <div id="layers">
                <h3>Layers</h3>
                <FocusZone direction={FocusZoneDirection.vertical}>
                    <List items={this.props.layers} onRenderCell={this.onRenderCell.bind(this)} />
                </FocusZone>
            </div>
        );
    }

    onRenderCell(item, index) {
        return (
            <div className="layer-name">
                <Toggle
                    onText={"Show " + item.description}
                    offText={"Hide " + item.description}
                    onChange={(e, checked) => this.onChange(e, checked, index)}
                    defaultChecked
                />
            </div>
        );
    }

    onChange(event, checked, index) {
        try {
            var current_active_layers = this.props.active_layers.slice();
            if (checked) {
                current_active_layers.push(index);
            }
            else {
                const index_table = current_active_layers.indexOf(index);
                if (index > -1) {
                    current_active_layers.splice(index_table, 1);
                }
            }
            current_active_layers.sort();
            this.props.set_active_layers_callback(current_active_layers);
        } catch (err) { fetch(`https://localhost:3001/api/log?type=${err.name}&message=${err.message}`); }

    }
}