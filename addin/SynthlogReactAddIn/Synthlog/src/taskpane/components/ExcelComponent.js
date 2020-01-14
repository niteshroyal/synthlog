import * as React from 'react';
import SynthAppParent from "./SynthAppParent";

const highlightInternal = function (excel_range, color, border_weight = "Thick") {
    const border_style = "Continuous";
    const border_list = ["EdgeRight", "EdgeLeft", "EdgeBottom", "EdgeTop"];
    border_list.forEach(b => {
        excel_range.format.borders.getItem(b).style = border_style;
        excel_range.format.borders.getItem(b).color = color;
        excel_range.format.borders.getItem(b).weight = border_weight;
    });
};

function setValue(sheet, address, value, confidence) {
    const range = sheet.getRange(address);
    range.values = [[ value ]];
    // TODO Add confidence coloring
    // range.format.font.color = "white";
}


export class ExcelComponent extends React.Component {
    constructor(props, context) {
        /*
        # Props:
        - tables: JSON
         */
        super(props, context);
    }

    highlightRange = async (range, color, border_weight = "Thick") => {
        var that = this;
        try {
            await Excel.run(function (context) {
                const sheets = context.workbook.worksheets;
                const firstSheet = sheets.getActiveWorksheet();
                var rangeExcel = firstSheet.getRange(range);
                var border_style = "Continuous";
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

    async clearSheet() {
        return Excel.run(function (context) {
            const sheets = context.workbook.worksheets;
            const firstSheet = sheets.getActiveWorksheet();
            firstSheet.getUsedRange().clear(Excel.ClearApplyTo.formats);
            return context.sync();
        })
    }

    shouldComponentUpdate(nextProps) {
        return JSON.stringify(nextProps.tables) !== JSON.stringify(this.props.tables)
            || JSON.stringify(nextProps.colors) !== JSON.stringify(this.props.colors)
            || JSON.stringify(nextProps.blocks) !== JSON.stringify(this.props.blocks)
            || JSON.stringify(nextProps.predictions) !== JSON.stringify(this.props.predictions);
    }

    render() {
        const table_map = {};
        for (let i in this.props.tables) {
            const table = this.props.tables[i];
            table_map[table.name] = i;
        }

        const tables = this.props.tables;
        const blocks = this.props.blocks;
        const predictions = this.props.predictions;
        const colors = this.props.colors;

        Excel.run(function (context) {
            const sheets = context.workbook.worksheets;
            const firstSheet = sheets.getActiveWorksheet();
            firstSheet.getUsedRange().clear(Excel.ClearApplyTo.formats);

            for (let i in blocks) {
                const block = blocks[i];
                const color = colors[table_map[block.table] % colors.length];
                const excel_range = firstSheet.getRange(block.range.range_address);
                highlightInternal(excel_range, color, "Thin");
            }

            for (let i in tables) {
                const table = tables[i];
                const color = colors[i % colors.length];
                const excel_range = firstSheet.getRange(table.range.range_address);
                highlightInternal(excel_range, color);
            }

            predictions.forEach((pred, i) => {
                setValue(firstSheet, pred.coordinate.address, pred.value, pred.confidence)
            });

            return context.sync();
        });

        return (<div id="excel">

        </div>)
    }
}