import * as UIElements from './UIElements';
import * as React from 'react';

const highlightBorder = function (excel_range, color, border_weight, border_style, border) {
    if (border_style != null) {
        excel_range.format.borders.getItem(border).style = border_style;
    }
    if (color != null) {
        excel_range.format.borders.getItem(border).color = color;
    }
    if (border_weight != null) {
        excel_range.format.borders.getItem(border).weight = border_weight;
    }
};

export class ExcelRenderer extends React.Component {
    constructor(props, context) {
        super(props, context);
    }

    render() {
        var that = this;
        this.clearSheet();
        // this.clearElements(elements);
        Excel.run(function (context) {
            that.props.elements.forEach(element => {
                that.renderElement(element, context);
            });
            return context.sync();

        });
        return (<div id="excel">

        </div>)
    };

    shouldComponentUpdate(nextProps) {
        return JSON.stringify(nextProps.elements) !== JSON.stringify(this.props.elements);
    }

    renderElement(element, context) {
        try {
            if (element instanceof UIElements.Range || element instanceof UIElements.Cell) {
                const sheets = context.workbook.worksheets;
                const firstSheet = sheets.getActiveWorksheet();

                var rangeExcel = element instanceof UIElements.Range ? firstSheet.getRange(element.range_address) : firstSheet.getRange(element.cell_address);
                // We do the borders first
                // TODO: Generic function for border coloring
                if (element.formatting != null) {
                    if (element.formatting.borders != null) {
                        for (const [border_name, border_format] of Object.entries(element.formatting.borders)) {
                            highlightBorder(rangeExcel, border_format.color, border_format.weight, border_format.style, border_name);
                        }
                    }
                    if (element.formatting.fill != null) {
                        if (element.formatting.fill.color != null) {
                            rangeExcel.format.fill.color = element.formatting.fill.color;
                        }
                    }
                    if (element.formatting.font != null) {
                        if (element.formatting.font.color != null) {
                            rangeExcel.format.font.color = element.formatting.font.color;
                        }
                        if (element.formatting.font.bold != null) {
                            rangeExcel.format.font.bold = element.formatting.font.bold;
                        }
                        if (element.formatting.font.italic != null) {
                            rangeExcel.format.font.italic = element.formatting.font.italic;
                        }
                        if (element.formatting.font.name != null) {
                            rangeExcel.format.font.name = element.formatting.font.name;
                        }
                        if (element.formatting.font.size != null) {
                            rangeExcel.format.font.size = element.formatting.font.size;
                        }
                        if (element.formatting.font.underline != null) {
                            rangeExcel.format.font.underline = element.formatting.font.underline;
                        }
                    }
                }
            }

            if (element instanceof UIElements.Cell) {
                if (element.value != null) {
                    rangeExcel.values = [[element.value]];
                }
            }
        } catch (err) { fetch(`https://localhost:3001/api/log?type=${err.name}&message=${err.message}`); }
    }

    clearElements = async (elements) => {
        await Excel.run(function (context) {
            elements.forEach(element => {
                that.clearElement(element, context);
            });
            return context.sync();

        });
    }

    clearElement(element, context) {
        if (element instanceof UIElements.Range || element instanceof UIElements.Cell) {
            const sheets = context.workbook.worksheets;
            const firstSheet = sheets.getActiveWorksheet();

            var rangeExcel = element instanceof UIElements.Range ? firstSheet.getRange(element.range_address) : firstSheet.getRange(element.cell_address);

            rangeExcel.clear(Excel.ClearApplyTo.formats);
        }
    }

    async clearSheet() {
        return Excel.run(function (context) {
            const sheets = context.workbook.worksheets;
            const firstSheet = sheets.getActiveWorksheet();
            firstSheet.getUsedRange().clear(Excel.ClearApplyTo.formats);
            return context.sync();
        })
    }
}