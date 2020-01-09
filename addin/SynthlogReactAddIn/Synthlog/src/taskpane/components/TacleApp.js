import * as React from 'react';
import 'isomorphic-fetch';
import SynthAppParent from './SynthAppParent';
import TableViewer from './TableViewer';
// import {TacleComponent} from "./TacleComponent";
import {ExcelComponent} from "./ExcelComponent";


export default class TacleApp extends SynthAppParent {
    constructor(props, context) {
        super(props, context);
    }

    componentDidMount() {
        super.componentDidMount();
        this.setState({task_suggestions_enabled: false});
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
                        loadTablesFn={this.loadTablesCallback.bind(this)}
                    />
                    {/*<TacleComponent />*/}
                    <ExcelComponent tables={this.state.tables} colors={this.colors} />
                </div>
            );
        }
    }
}