import * as React from 'react';
import { Button, ButtonType } from 'office-ui-fabric-react';
import { ChoiceGroup, IChoiceGroupOption } from 'office-ui-fabric-react/lib/ChoiceGroup';

export class DatasetInteractiveBuild extends React.Component {
    constructor(props, context) {
        super(props, context);
        this.state = {
            label_option: "pos",
            message: "",
            negative_ranges: [],
            positive_ranges: [],
        };
        this.parent = null;
    }

    render() {
        const {
            parent
        } = this.props;
        this.parent = parent;
        return (
            <div id='dataset_interactive'>
                <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.addExample.bind(this)}>Add Example</Button>
                <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.inferDataset.bind(this)}>Infer Dataset</Button>
                <ChoiceGroup
                    className="modeChoice"
                    defaultSelectedKey="pos"
                    options={[
                        {
                            key: "pos",
                            text: "Positive Examples"
                        },
                        {
                            key: "neg",
                            text: "Negative Examples"
                        },
                    ]}
                    onChange={this.changeMode.bind(this)}
                    label="Choose Example Labeling"
                    required={true}
                />
                <p>
                    {this.state.message}
                </p>
                <hr />
            </div>
        );
    }

    addExample() {
        var that = this;
        /* Sets the current range to be the train set */
        Excel.run(context => {
            var range = context.workbook.getSelectedRanges();
            var ranges = []

            range.load(['areas']);
            range.areas.load(['items']);
            return context.sync().then(function () {
                ranges = range.areas.items;
                if (that.state.label_option == "pos") {
                    range.format.fill.color = '#0080FF';
                    for (var i = 0; i < ranges.length; i++) {
                        var r = ranges[i];
                        try {
                            that.getRangeAddress(context, r).then(function (address) {
                                var new_pos = that.state.positive_ranges.concat([address]);
                                that.setState({ positive_ranges: new_pos });
                            });
                        } catch (err) { fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`) };
                    }
                }
                else {
                    range.format.fill.color = '#FE2E2E';
                    for (var i = 0; i < ranges.length; i++) {
                        var r = ranges[i];
                        try {
                            that.getRangeAddress(context, r).then(function (address) {
                                var new_neg = that.state.negative_ranges.concat([address]);
                                that.setState({ negative_ranges: new_neg });
                            });
                        } catch (err) { fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`) };
                    }
                }
            });
        })
    }

    getRangeAddress = async (context, range) => {
        range.load(['address']);
        await context.sync();
        return range.address;
    }

    changeMode(ev, option) {
        this.setState({ label_option: option.key });
    }

    inferDataset() {
        var that = this;
        this.setState({ message: "inferring! pos: " + String(this.state.positive_ranges.length) + " neg: " + String(this.state.negative_ranges.length) });

        var parameters = { file: Office.context.document.url, relevant_ranges: this.state.positive_ranges, unrelevant_ranges: this.state.negative_ranges, tables:this.parent.state.tables };
        fetch(`${that.parent.api}/extend_relevant`, {
            method: 'POST',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(parameters)
        }).then(response => response.json())
            .then(function (json) {
                // Do something with respons
                // Color new ranges
                // Add new interactions based on answer?
                try {
                    // We color new extended ranges for positive (relevant) examples
                    if (json.extended_relevant.length > 0) {
                        json.extended_relevant.forEach(function (range) {
                            fetch(`${that.parent.api}/log?type=range&message=${range}`);
                            var sheet_name = range.split("!")[0];
                            var actual_range = range.split("!")[1];
                            Excel.run(function (context) {
                                var excel_range = context.workbook.worksheets.getItem(sheet_name).getRange(actual_range);
                                excel_range.format.fill.color = "#87CEFA"
                                return context.sync();
                            })
                        })
                    }
                    // We also color unrelevant examples
                    if (json.extended_unrelevant.length > 0) {
                        json.extended_unrelevant.forEach(function (range) {
                            fetch(`${that.parent.api}/log?type=range&message=${range}`);
                            var sheet_name = range.split("!")[0];
                            var actual_range = range.split("!")[1];
                            Excel.run(function (context) {
                                var excel_range = context.workbook.worksheets.getItem(sheet_name).getRange(actual_range);
                                excel_range.format.fill.color = "#FA8072"
                                return context.sync();
                            })
                        })
                    }
                    // Other types of interaction?
                } catch (err) { fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`) };
            });

    }
}