from problog.logic import Term, Constant, Object, list2term


def init_cell(row, column, value, p=None):
    """
    Initialize a cell predicate
    :param row: The cell row ID
    :type row: int

    :param column: The cell column ID
    :type column: int

    :param value: The cell value
    :type value: str

    :param p: The cell probability (optional)
    :type p: float

    :return: The cell Term
    :rtype: Problog Term
    """
    return Term("cell", Constant(row), Constant(column), Constant(value), p=p)


def init_cell_pred(row, column, value, prediction_obj, p=None):
    """
    Initialize a cell predicate
    :param row: The cell row ID
    :type row: int

    :param column: The cell column ID
    :type column: int

    :param value: The cell value
    :type value: str

    :param p: The cell probability (optional)
    :type p: float

    :return: The cell Term
    :rtype: Problog Term
    """
    return Term(
        "cell_pred",
        Constant(row),
        Constant(column),
        Constant(value),
        prediction_obj,
        p=p,
    )


def init_cell_transform(row, column, value, transformation_id, p=None):
    """
    Initialize a cell_transform predicate
    :param row: The cell row ID
    :type row: int

    :param column: The cell column ID
    :type column: int

    :param value: The cell value
    :type value: str

    :param p: The cell probability (optional)
    :type p: float

    :return: The cell Term
    :rtype: Problog Term
    """
    return Term(
        "cell_transform",
        Constant(row),
        Constant(column),
        Constant(value),
        Constant(transformation_id),
        p=p,
    )


def init_table(name, first_row, first_column, row_number, column_number):
    """
    Initialize a table predicate
    :param name: table name
    :type name: str

    :param first_row: first row ID
    :type first_row: int

    :param first_column: first column ID
    :type first_column: int

    :param row_number: row number
    :type row_number: int

    :param column_number: column number
    :type column_number: int

    :return: The table Term
    :rtype: Problog Term
    """
    return Term(
        "table",
        Constant("'{}'".format(name)),
        Constant(first_row),
        Constant(first_column),
        Constant(row_number),
        Constant(column_number),
    )


def init_table_cell(table_name, row, column, value):
    """
    Initialize a table_cell predicate
    :param table_name: The name of the table containing this header
    :type table_name: str

    :param row: The header cell row ID
    :type row: int

    :param column: The header cell column ID
    :type column: int

    :param value: The header value
    :type value: str

    :return: The table_header predicate
    :rtype: Problog term
    """
    return Term(
        "table_cell",
        Constant("'{}'".format(table_name)),
        Constant(row),
        Constant(column),
        Constant(value),
    )


def init_table_cell_type(table_name, row, column, cell_type):
    """
    Initialize a table_cell_type predicate
    :param table_name: The name of the table containing this header
    :type table_name: str

    :param row: The cell row ID
    :type row: int

    :param column: The cell column ID
    :type column: int

    :param cell_type: The cell type
    :type cell_type: str

    :return: The table_cell_type predicate
    :rtype: Problog term
    """
    return Term(
        "table_cell_type",
        Constant("'{}'".format(table_name)),
        Constant(row),
        Constant(column),
        Constant(cell_type),
    )


def init_table_header(table_name, column, value, column_type, column_unique_values):
    """
    Initialize a table_header predicate
    :param table_name: The name of the table containing this header
    :type table_name: str

    :param column: The header cell column ID
    :type column: int

    :param value: The header value
    :type value: str

    :return: The table_header predicate
    :rtype: Problog term
    """
    return Term(
        "table_header",
        Constant("'{}'".format(table_name)),
        Constant(column),
        Constant(value),
        Constant(column_type),
        list2term(column_unique_values),
    )


def init_constraint(constraint):
    return Term("constraint", Object(constraint))
