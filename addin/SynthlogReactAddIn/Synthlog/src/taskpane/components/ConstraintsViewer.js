import * as React from 'react';
import {FocusZone, FocusZoneDirection} from 'office-ui-fabric-react/lib/FocusZone';
import {List} from 'office-ui-fabric-react/lib/List';
import {Label, Toggle} from "office-ui-fabric-react";

export default class ConstraintsViewer extends React.Component {
    constructor(props, context) {
        super(props, context);
        this.state = {
            show_functional: false
        }
    }

    render() {
        const constraints = this.props.constraints.filter((v) => this.state.show_functional || v.is_formula);
        return (
            <div id="constraints">
                <h3>Constraints</h3>
                <FocusZone direction={FocusZoneDirection.vertical}>
                    {this.props.constraints.length > 0 ? (<Toggle
                        onText="Show functional constraints"
                        offText="Hide functional constraints"
                        onChange={this.onChange.bind(this)}
                        checked={this.state.show_functional}
                      />) : ("")}
                    {/*<Button className='normal-button' buttonType={ButtonType.hero} onClick={this.detectTables.bind(this)}>Detect tables</Button>*/}
                    {/*<TextField label={'Filter by name'} onChange={this._onFilterChanged.bind(this)} />*/}
                    <List items={constraints} onRenderCell={this.onRenderCell.bind(this)}/>
                </FocusZone>
            </div>
        );
    }

    onRenderCell(item, index) {
        return (
            <div className="constraint-name">
                {item.name}
            </div>
        );
    }

    onChange(event, checked) {
        this.setState({show_functional: checked})
    }
}