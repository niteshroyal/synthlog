import * as React from 'react';
import 'isomorphic-fetch';
import SynthAppParent from './SynthAppParent';
import TableViewer from './TableViewer';
// import {TacleComponent} from "./TacleComponent";
import {ExcelComponent} from "./ExcelComponent";
import TasksComponent from "./TasksComponent";
import ConstraintsViewer from "./ConstraintsViewer";


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
            return (
                <div id='main' className='ms-welcome'>
                    <p>
                        {this.state.log}
                    </p>
                    <TableViewer
                        parent={this}
                        tables={this.state.tables}
                        colors={this.colors}
                    />
                    <ConstraintsViewer constraints={this.state.constraints} />
                    <TasksComponent tasks={this.state.tasks} callback={this.executeTask.bind(this)} />
                    {/*<TacleComponent />*/}
                    <ExcelComponent tables={this.state.tables} colors={this.colors} blocks={this.state.blocks} />
                </div>
            );
        }
    }
}