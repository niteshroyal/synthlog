import * as React from 'react';
import SynthAppParent from "./SynthAppParent";

export class ExcelComponent extends React.Component {
    constructor(props, context) {
        /*
        # Props:
        - tables: JSON
         */
        super(props, context);
    }

    highlightRange = async (range, color) => {
        var that = this;
        try {
            await Excel.run(function (context) {
                const sheets = context.workbook.worksheets;
                const firstSheet = sheets.getActiveWorksheet();
                var rangeExcel = firstSheet.getRange(range);
                var border_style = "Continuous";
                var border_weight = "Thick";
                var border_list = ["EdgeRight", "EdgeLeft", "EdgeBottom", "EdgeTop"];
                border_list.forEach(b => {
                    rangeExcel.format.borders.getItem(b).style = border_style;
                    rangeExcel.format.borders.getItem(b).color = color;
                    rangeExcel.format.borders.getItem(b).weight = border_weight;
                });
                return context.sync();
            })
                .catch(function (err) {
                    fetch(`${that.api}/log?type=${err.name}&message=${err.message}`);
                })
        } catch (err) {
            fetch(`${this.api}/log?type=${err.name}&message=${err.message}`);
        }
    };

    render() {
        for (let i in this.props.tables) {
            const table = this.props.tables[i];
            const color = this.props.colors[i % this.props.colors.length];
            const range = table.start_col + table.start_row + ":" + table.end_col + table.end_row;
            this.highlightRange(range, color);
        }

        return (<div id="excel">

        </div>)
    }
}