from abc import ABC, abstractmethod

class BaseAction(ABC):

    @abstractmethod
    def __init__(self, state):
        self.state = state

    @abstractmethod
    def undo(self):
        pass

    @abstractmethod
    def __str__(self):
        pass


class PutCellValue(BaseAction):
    def __init__(self, cell, value):
        self.cell = cell
        self.value = value

    def undo(self):
        pass

    def __str__(self):
        return "put_cell_value " + str(self.cell) + " " + str(self.value)

class PutRangeValues(BaseAction):
    def __init__(self, range, values):
        self.range = range
        self.values = values

    def undo(self):
        pass

    def __str__(self):
        return "put_range_value " + str(self.range) + " " + " ".join(self.values)

class PutRangeColor(BaseAction):
    def __init__(self, range, color):
        self.range = range
        self.color = color

    def undo(self):
        pass

    def __str__(self):
        return "put_range_color " + str(self.range) + " " + str(self.color)

class PutRangeFont(BaseAction):
    def __init__(self, range, color="", bold=False, italic=False, underline=False, size=-1, name=""):
        self.range = range
        self.color = color
        self.bold = bold
        self.italic = italic
        self.underline = underline
        self.size = size
        self.name = name

    def undo(self):
        pass

    def __str__(self):
        properties = []
        if self.color:
            properties.append("color=" + self.color)

        if self.bold:
            properties.append("bold")

        if self.italic:
            properties.append("italic")

        if self.underline:
            properties.append("underline")

        if self.size >= 0:
            properties.append("size=" + str(self.size))

        if self.name:
            properties.append("name="+self.name)
        return "put_range_font " + str(self.range) + " " + " ".join(properties)

class PutRangeBorder(BaseAction):
    def __init__(self, range, color="", style="", weight=""):
        self.range = range
        self.color = color
        self.style = style
        self.weight = weight

    def undo(self):
        pass

    def __str__(self):
        properties = []
        if self.color:
            properties.append("color=" + self.color)

        if self.style:
            properties.append("style="+self.style)

        if self.weight:
            properties.append("style="+self.weight)
        return "put_range_border " + str(self.range) + " " + " ".join(properties)