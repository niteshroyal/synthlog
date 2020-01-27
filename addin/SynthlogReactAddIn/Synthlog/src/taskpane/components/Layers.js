import * as UIElements from './UIElements';

export class Layer {
    constructor(description) {
        this.colors = ["#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b", "#eeca3b", "#b279a2", "#ff9da6", "#9d755d", "#bab0ac"];
        this.description = description
    }

    augmentState(state) {
        return state
    }

    getUIElements(state) {
        return [];
    }
}

export class TableLayer extends Layer {
    constructor() {
        super("Table Layer");
    }

    augmentState(json_state) {
        json_state = super.augmentState(json_state);
        json_state.tables.forEach((table, index) => {
            if (table.range.formatting === null) {
                const border_formatting = new UIElements.BorderFormatting(this.colors[index % this.colors.length], "Continuous", "Thick");
                const border_formattings = {
                    "EdgeRight": border_formatting,
                    "EdgeLeft": border_formatting,
                    "EdgeBottom": border_formatting,
                    "EdgeTop": border_formatting
                };
                table.range.formatting = new UIElements.ObjectFormatting(null, null, border_formattings);
            }
        });
        // TODO Update state in database by adding formatting
        return json_state
    }

    getUIElements(state) {
        return state.tables.map(function (table) {
            const new_range = {};
            new_range["range_address"] = table.range.range_address;
            new_range["formatting"] = table.range.formatting;
            new_range["values"] = table.range.values;
            return new UIElements.Range(new_range["range_address"], new_range["values"], new_range["formatting"]);
        });
    }
}

export class BlockLayer extends Layer {
    constructor() {
        super("Block Layer");
    }

    getUIElements(state) {
        return state.blocks.map((block) => {
            let new_range = {};
            let table_formatting = this.getTableRangeFormatting(state.tables, block.table);
            new_range["range_address"] = block.range.range_address;
            new_range["formatting"] = block.range.formatting;
            new_range["values"] = block.range.values;

            if (new_range["formatting"] === null) {
                if (table_formatting != null) {
                    const border_formatting = new UIElements.BorderFormatting(table_formatting.borders["EdgeRight"].color, "Continuous", "Thin");
                    const border_formattings = {
                        "EdgeRight": border_formatting,
                        "EdgeLeft": border_formatting,
                        "EdgeBottom": border_formatting,
                        "EdgeTop": border_formatting
                    };
                    new_range["formatting"] = new UIElements.ObjectFormatting(null, null, border_formattings);
                }
            }
            return new UIElements.Range(new_range["range_address"], new_range["values"], new_range["formatting"]);
        });
    }

    getTableRangeFormatting(tables, table_name) {
        let formatting = null;
        tables.forEach(table => {
            if (table.name === table_name) {
                formatting = table.range.formatting
            }
        });
        return formatting;
    }
}

export class BlockTableLayer extends Layer {
    constructor() {
        super("Table and Block Layer");
        this.table_layer = new TableLayer();
        this.block_layer = new BlockLayer();
    }

    augmentState(json_state) {
        json_state = super.augmentState(json_state);
        return this.block_layer.augmentState(this.table_layer.augmentState(json_state))
    }

    getUIElements() {
        // First get table elements to update the state with default choices for colors
        const table_elements = this.table_layer.getUIElements();

        // We want to first display the elements of blocks and then tables, to keep thick table borders
        return this.block_layer.getUIElements().concat(table_elements);
    }
}

export class PredictionLayer extends Layer {
    constructor() {
        super("Prediction Layer");
        this.confidence_colors = ["#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850", "#006837"];
    }

    getUIElements(state) {
        return state.predictions.map((pred) => {
            let fill_format = null;
            if (pred.confidence != null) {
                fill_format = new UIElements.FillFormatting(this.getConfidenceColor(pred.confidence));
            }
            const format = new UIElements.ObjectFormatting(fill_format, null, null);
            // noinspection JSUnresolvedVariable
            return new UIElements.Cell(pred.coordinate.address, pred.value, format);

        });
    }

    getConfidenceColor(conf) {
        // Confidence has to be normalized between 0 and 1
        if (conf >= 1) {
            return this.confidence_colors[this.confidence_colors.length - 1];
        }
        return this.confidence_colors[Math.floor(conf * this.confidence_colors.length)];
    }
}

export class SelectionLayer extends Layer {
    constructor() {
        super("Selection Layer");
    }

    getUIElements(state) {
        return state.selections.map((sel) => {
            const fill_format = new UIElements.FillFormatting(sel.cell.formatting.fill.color);

            const format = new UIElements.ObjectFormatting(fill_format, null, null);
            // noinspection JSUnresolvedVariable
            return new UIElements.Cell(sel.cell.cell_address, null, format);

        });
    }
}