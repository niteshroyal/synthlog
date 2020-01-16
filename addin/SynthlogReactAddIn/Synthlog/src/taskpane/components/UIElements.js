export class FillFormatting {
    constructor(color) {
        this.color = color
    }
}
export class FontFormatting {
    constructor(bold, color, italic, name, size, underline) {
        this.bold = bold
        this.color = color
        this.italic = italic
        this.name = name
        this.size = size
        this.underline = underline
    }

}

export class BorderFormatting {
    constructor(color, style, weight) {
        this.color = color
        this.style = style
        this.weight = weight
    }
}


export class ObjectFormatting {
    constructor(
        fill,
        font,
        borders
    ) {
        this.fill = fill
        this.borders = borders
        this.font = font

    }
}


export class Cell {
    constructor(
        cell_address,
        value,
        formatting,
    ) {
        this.cell_address = cell_address
        this.formatting = formatting
        this.value = value
    }
}


export class Range {
    constructor(
        range_address,
        values,
        formatting,
    ) {
        this.range_address = range_address
        this.formatting = formatting
        this.values = values

    }
}