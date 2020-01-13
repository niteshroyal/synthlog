import * as React from 'react';
import {FocusZone, FocusZoneDirection} from 'office-ui-fabric-react/lib/FocusZone';
import {List} from 'office-ui-fabric-react/lib/List';

export default class ConstraintsViewer extends React.Component {
    constructor(props, context) {
        super(props, context);
    }

    render() {
        return (
            <div id="constraints">
                <h3>Constraints</h3>
                <FocusZone direction={FocusZoneDirection.vertical}>
                    {/*<Button className='normal-button' buttonType={ButtonType.hero} onClick={this.detectTables.bind(this)}>Detect tables</Button>*/}
                    {/*<TextField label={'Filter by name'} onChange={this._onFilterChanged.bind(this)} />*/}
                    <List items={this.props.constraints} onRenderCell={this._onRenderCell.bind(this)}/>
                </FocusZone>
            </div>
        );
    }

    _onRenderCell(item, index) {
        return (
            <div className="constraint-name">
                {item.name}
            </div>
        );
    }
}