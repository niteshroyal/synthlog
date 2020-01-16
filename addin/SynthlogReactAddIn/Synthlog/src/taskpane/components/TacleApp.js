import * as React from 'react';
import 'isomorphic-fetch';
import SynthAppParent from './SynthAppParent';
import TableViewer from './TableViewer';
// import {TacleComponent} from "./TacleComponent";
import { ExcelRenderer } from "./ExcelRenderer";
import TasksComponent from "./TasksComponent";
import ConstraintsViewer from "./ConstraintsViewer";
import ActivityLog from "./ActivityLog";
import LayersViewer from "./LayersViewer";
import PredictionsViewer from "./PredictionsViewer";


export default class TacleApp extends SynthAppParent {
    constructor(props, context) {
        super(props, context);
    }

    componentDidMount() {
        super.componentDidMount();
    }

    render() {
        if (this.state.loading) {
            return (
                <div id='main' className='ms-welcome'>
                    <p>
                        {this.state.log}
                    </p>
                    Loading...
                </div>
            )
        } else {
            super.renderLayers();
            return (
                <div id='main' className='ms-welcome'>
                    <p>
                        {this.state.log}
                    </p>
                    <TableViewer
                        parent={this}
                        tables={this.state.tables}
                    />
                    <ConstraintsViewer constraints={this.state.constraints} />
                    <PredictionsViewer predictions={this.state.predictions} />
                    <TasksComponent tasks={this.state.tasks} callback={this.executeTask.bind(this)} loading={this.state.loading_tasks} />
                    <ActivityLog activities={this.state.activities} loadState={this.loadStateFromId.bind(this)} />
                    <ExcelRenderer
                        elements={this.uiElements}
                    />
                    <LayersViewer layers={this.layers} active_layers={this.state.active_layers} update_state_func={this.setStatesync.bind(this)}/>
                    {/*<TacleComponent />*/}
                    {/* <ExcelComponent
                        tables={this.state.tables}
                        colors={this.colors}
                        blocks={this.state.blocks}
                        predictions={this.state.predictions}
                    /> */}
                </div>
            );
        }
    }
}