import * as React from 'react';
import { Button, ButtonType } from 'office-ui-fabric-react';
import { any } from 'prop-types';
import { isArray } from 'util';

export class PredictionDiv extends React.Component {
    constructor(props, context) {
        super(props, context);
        this.state={
            message:"hey"
        };
        this.features = {
            columns:[],
            rows:[],
            values:[],
            valueTypes:[]
        };
        this.ranges_to_pred = {
            columns:[],
            rows:[],
            values:[],
            valueTypes:[]
        };
        this.parent = null;
    }

    render() {
        const {
            parent
        } = this.props;
        this.parent = parent;
        return (
            <div id='prediction'>
                    <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.setFeatures.bind(this)}>Set Features</Button>
                    <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.setRangeToPred.bind(this)}>To Predict</Button>
                    <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.predict.bind(this)}>Predict!</Button>
                    <p>
                        {this.state.message}
                    </p>
                    
            </div>
        );
    }

    getRangeColumnsAndRows(context, range, that, col_container, row_container, values_container, valueTypes_container) {
        return context.sync().then(function () {
            range.load(['areas']);
            return context.sync();
        })
            .then(function () {
                range.areas.items.forEach(function (e) {
                    var area_columns = [];
                    var area_rows = [];
                    for (var i = e.columnIndex; i < e.columnIndex + e.columnCount; i++) {
                        area_columns.push(i);
                    }
                    for (var i = e.rowIndex; i < e.rowIndex + e.rowCount; i++) {
                        area_rows.push(i);
                    }
                    col_container.push(area_columns);
                    row_container.push(area_rows);

                    try{
                        values_container.push(e.values);
                        valueTypes_container.push(e.valueTypes);
                    }
                    catch(err){
                        fetch(`https://localhost:3001/api/log?type=${err.name}&message=${err.message}`);
                    }
                });
                that.setState({ message: values_container.length + "_" + row_container + "_" + col_container});
                
                return range.context.sync().then().catch(err => fetch(`https://localhost:3001/api/log?type=${err.name}&message=${err.message}`));
            });
    }

    setFeatures() {
        var that = this;
        /* Sets the current range to be the train set */
       Excel.run(context => {
             var range = context.workbook.getSelectedRanges();
             range.format.fill.color = '#6da7de';

             that.features.columns = [];
             that.features.rows = [];
             that.features.values = [];
             that.features.valueTypes = [];

             return this.getRangeColumnsAndRows(context, range, that, that.features.columns, that.features.rows, that.features.values, that.features.valueTypes);
         })
    }

    setRangeToPred() {
        var that = this;
        /* Sets the current range to be the train set */
       Excel.run(context => {
             var range = context.workbook.getSelectedRanges();
             range.format.fill.color = '#943fa6';

             that.ranges_to_pred.columns = [];
             that.ranges_to_pred.rows = [];
             that.ranges_to_pred.values = [];
             that.ranges_to_pred.valueTypes = [];

             return this.getRangeColumnsAndRows(context, range, that, that.ranges_to_pred.columns, that.ranges_to_pred.rows, that.ranges_to_pred.values, that.ranges_to_pred.valueTypes);
         })
    }

    predict = async() => {
    //     var all_values = [];
    //     var row_lengths = this.features.values.map(row => row.length);
    //     var max_row_lengths = Math.max(...row_lengths);


    //    for(var i=0; i<max_row_lengths; i++){
    //        var row = [];
    //        for(var j=0; j<this.features.values; j++){
    //            for(var k=0; k<this.features.values[j]; k++){
    //                if(this.features.values[j][k].length > i)
    //                 row.push(this.features.values[j][k][i]);
    //            }
    //        }
    //        all_values.push(row);
    //    }

    //     this.setState({ message: all_values});
        try{
            var that = this;
        var parameters = {
            scope: this.parent.state.active,
            script: "builtin/prediction.pl",
            train_row: this.features.rows.reduce((acc, val) => acc.concat(val), []).map(x => [x]),
            train_column: this.features.columns.reduce((acc, val) => acc.concat(val), []).map(x => [x]),
            pred_row: this.ranges_to_pred.rows.reduce((acc, val) => acc.concat(val), []).map(x => [x]),
            pred_column: this.ranges_to_pred.columns.reduce((acc, val) => acc.concat(val), []).map(x => [x]),
          };
        parameters = this.parent.generateSynthlogParameters(parameters);
        this.parent.runSynthlog(parameters).then(function(json) {
            // fetch(`https://localhost:3001/api/log?type=test&message=${json.output}`);
            if (json.output) {
                try {
                    // that.parent.clearSpreadsheet();
                    var cells = that.parseResult(json.output);
                    that.parent.fillSpreadsheet(cells);
                }
                catch(err) {
                    fetch(`https://localhost:3001/api/log?type=${err.name}&message=${err.message}`);
                }
            }
        }
    )
        }
        catch(err){
            fetch(`https://localhost:3001/api/log?type=${err.name}&message=${err.message}`);
        }

    }

    parseResult(result) {
        var cells = []
        var regex = /table_cell\(('.+?'),([0-9]+),([0-9]+),(.+?)\):(.+)/;
        var that = this;

        var flattened_pred_rows = this.ranges_to_pred.rows.reduce((acc, val) => acc.concat(val), []);
        var flattened_pred_cols = this.ranges_to_pred.columns.reduce((acc, val) => acc.concat(val), []);

        // fetch(`https://localhost:3001/api/log?type=base&message=${flattened_pred_rows}`);
        // fetch(`https://localhost:3001/api/log?type=base&message=${flattened_pred_cols}`);

        // add proba
        result.forEach(function(element) {
            var m = element.match(regex);
            fetch(`https://localhost:3001/api/log?type=reg&message=${m}`);
            // fetch(`https://localhost:3001/api/log?type=reg&message=${element}`)
            if(m){
                // Results from Synthlog are 1,1 based, and only contained rows and columns specified from ranges_to_pred.
                // The next 2 lines perform the mapping between the SynthLog indices and the Excel indices                
                var row = flattened_pred_rows[parseInt(m[2])-1];
                var col = flattened_pred_cols[parseInt(m[3])-1];

                // fetch(`https://localhost:3001/api/log?type=it&message=${row}`);
                // fetch(`https://localhost:3001/api/log?type=it&message=${col}`);

                cells.push([row, col, m[4].replace(/'/g, ''), parseFloat(m[5])]);
            }
        });
        this.setState({message: cells});
        return cells;
    }
}