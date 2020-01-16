import * as UIElements from './UIElements';

export class Layer {
    constructor(state) {
        this.state = state;
        this.colors = ["#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b", "#eeca3b", "#b279a2", "#ff9da6", "#9d755d", "#bab0ac"];
    }

    updateState(state) {
        this.state = state;
    }

    getUIElements() {
        return [];
    }
}

export class TableLayer extends Layer {
    constructor(state, setStatesync) {
        super(state);
        this.setStatesync = setStatesync;
    }

    getUIElements() {
        var uiElems = [];

        var that = this;
        var new_formatting = false;
        var table_copies = this.state.tables.slice();

        this.state.tables.forEach(function (table, index) {
            var new_range = {};
            new_range["range_address"] = table.range.range_address;
            new_range["formatting"] = table.range.formatting;
            new_range["values"] = table.range.values

            if (new_range["formatting"] === null) {
                new_formatting = true;
                var border_formatting = new UIElements.BorderFormatting(that.colors[index % that.colors.length], "Continuous", "Thick");
                var border_formattings = { "EdgeRight": border_formatting, "EdgeLeft": border_formatting, "EdgeBottom": border_formatting, "EdgeTop": border_formatting };

                var object_formatting = new UIElements.ObjectFormatting(null, null, border_formattings);
                new_range["formatting"] = object_formatting;

                table_copies[index].range.formatting = new_range["formatting"];

            }
            uiElems.push(new UIElements.Range(new_range["range_address"], new_range["values"], new_range["formatting"]));
        });

        if (new_formatting) {
            this.setStatesync({ tables: table_copies });
        }
        fetch(`https://localhost:3001/api/log?type=uielems_table&message=${JSON.stringify(uiElems)}`);

        return uiElems;
    }
}

export class BlockLayer extends Layer {
    constructor(state) {
        super(state);
    }

    getUIElements() {
        var uiElems = [];
        var that = this;

        this.state.blocks.forEach(function (block) {
            var new_range = {};
            var table_formatting = that.getTableRangeFormatting(block.table);
            new_range["range_address"] = block.range.range_address;
            new_range["formatting"] = block.range.formatting;
            new_range["values"] = block.range.values;

            if (new_range["formatting"] === null) {
                if (table_formatting != null) {
                    var border_formatting = new UIElements.BorderFormatting(table_formatting.borders["EdgeRight"].color, "Continuous", "Thin");
                    var border_formattings = { "EdgeRight": border_formatting, "EdgeLeft": border_formatting, "EdgeBottom": border_formatting, "EdgeTop": border_formatting };

                    var object_formatting = new UIElements.ObjectFormatting(null, null, border_formattings);
                    new_range["formatting"] = object_formatting;
                }
            }
            uiElems.push(new UIElements.Range(new_range["range_address"], new_range["values"], new_range["formatting"]));
        });

        fetch(`https://localhost:3001/api/log?type=uielems_blocks&message=${JSON.stringify(uiElems)}`);

        return uiElems;
    }

    getTableRangeFormatting(table_name) {
        var formatting = null;
        this.state.tables.forEach(table => {
            if (table.name == table_name) {
                formatting = table.range.formatting
            }
        });

        return formatting;
    }
}

export class BlockTableLayer extends Layer {
    constructor(state, setStatesync) {
        super(state);
        this.table_layer = new TableLayer(state, setStatesync);
        this.block_layer = new BlockLayer(state);
    }

    updateState(state) {
        super.updateState(state);
        this.table_layer.updateState(state);
        this.block_layer.updateState(state);
    }

    getUIElements() {
        var uiElems = [];
        // First get table elements to update the state with default choices for colors        
        var table_elems = this.table_layer.getUIElements();
        uiElems = this.block_layer.getUIElements();

        // We want to first display the elements of blocks and then tables, to keep thick table borders
        uiElems = uiElems.concat(table_elems);


        return uiElems;
    }
}