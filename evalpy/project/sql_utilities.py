from typing import Dict, List
from enum import Enum
from collections import defaultdict, namedtuple

import sqlite3

type_translation = defaultdict(lambda: 'blob', {int: 'integer', str: 'text', float: 'real', bool: 'integer'})
sql_where_filter = namedtuple('SQLWhereFilter', ['junction', 'entry', 'operator', 'value', 'start_bracket',
                                                 'end_bracket'])


class SQLJunction(Enum):
    AND = 'AND'
    OR = 'OR'
    NONE = ''


class SQLOperator(Enum):
    GREATER = '>'
    GREATER_EQUALS = '>='
    EQUALS = '=='
    NOT_EQUALS = '!='
    LESSER = '<'
    LESSER_EQUALS = '<='


def escape(string: str):
    return string.replace(' ', '_')


def establish_connection(db_file: str):
    connection = sqlite3.connect(db_file)
    create_main_table(connection)
    return connection


def create_main_table(connection):
    cursor = connection.cursor()
    sql_statement = """ CREATE TABLE IF NOT EXISTS params (
                                            run_id text PRIMARY KEY,
                                            experiment_name text NOT NULL
                                        ); """
    cursor.execute(sql_statement)


def create_run_table(connection, table_name):
    cursor = connection.cursor()
    sql_statement = f""" CREATE TABLE IF NOT EXISTS {table_name} (
                                            step integer PRIMARY KEY
                                        ); """
    cursor.execute(sql_statement)


def add_row_to_main_table(connection, params: Dict):
    _correct_column_scheme(connection, params, 'params')
    cursor = connection.cursor()
    _add_data_row_to_table(cursor, params, 'params')


def add_row_to_run_table(connection, run_id, params):
    _correct_column_scheme(connection, params, run_id)
    cursor = connection.cursor()
    _add_data_row_to_table(cursor, params, run_id)


def get_column_values(connection, column_name):
    cursor = connection.cursor()
    a = cursor.execute(f'''SELECT {escape(column_name)} FROM params''')
    ret = a.fetchall()
    return ret


def get_column_values_filtered(connection, target_columns: List[str], table: str,
                               where_filters: List[sql_where_filter]):
    columns = ', '.join([escape(c) for c in target_columns])
    cursor = connection.cursor()
    table = escape(table)
    where_clause = _build_where_clause_with_groups(where_filters)
    sql_statement = f'SELECT {columns} FROM {table} {where_clause}'
    cursor = cursor.execute(sql_statement)
    return cursor.fetchall()


def delete_rows(connection, table, where_filters):
    cursor = connection.cursor()
    table = escape(table)
    where_clause = _build_where_clause_with_groups(where_filters)
    sql_statement = f'DELETE FROM {table} {where_clause}'
    cursor.execute(sql_statement)


def delete_table(connection, table):
    cursor = connection.cursor()
    table = escape(table)
    if table == 'params':
        raise ValueError('Dropping the table params is not allowed')
    sql_statement = f'DROP TABLE IF EXISTS {table}'
    cursor.execute(sql_statement)


def _correct_column_scheme(connection, params, table_name):
    cursor = connection.cursor()
    current_columns = get_current_columns_of_table(connection, table_name)
    columns_to_be_created = set([k for k in params.keys() if escape(k) not in set(current_columns)])

    for column_name in columns_to_be_created:
        sql_statement = f"ALTER TABLE {table_name} ADD COLUMN {escape(column_name)} " \
                        f"{type_translation[type(params[column_name])]}"
        cursor.execute(sql_statement)


def get_current_columns_of_table(connection, table_name):
    cursor = connection.execute(f'select * from {table_name}')
    return [description[0] for description in cursor.description]


def _add_data_row_to_table(cursor, params, table_name):
    columns = ', '.join([escape(k) for k in params.keys()])
    placeholders = ', '.join('?' * len(params))
    sql = 'INSERT INTO {} ({}) VALUES ({})'.format(table_name, columns, placeholders)
    param_values = [int(v) if isinstance(v, bool) else v for v in params.values()]
    try:
        cursor.execute(sql, param_values)
    except sqlite3.OperationalError as e:
        print(f'The following statement could not be resolved: {sql} || param values: {param_values}')
        raise e


def _build_where_clause_with_groups(where_filters: List[sql_where_filter]):
    if not where_filters:
        return ''
    sql_clause = 'WHERE '
    for sql_filter in where_filters:
        sql_clause += translate_sql_filter(sql_filter)
    return sql_clause


def translate_sql_filter(sql_filter):
    start_bracket_eval = {True: '(', False: ''}
    end_bracket_eval = {True: ')', False: ''}
    return f'{sql_filter.junction.value} {start_bracket_eval[sql_filter.start_bracket]}{escape(sql_filter.entry)} ' \
           f'{sql_filter.operator.value} {_interpret_value_type(sql_filter.value)}' \
           f'{end_bracket_eval[sql_filter.end_bracket]} '


def _interpret_value_type(value):
    try:
        return int(value)
    except ValueError:
        try:
            return float(value)
        except ValueError:
            return f"'{value}'"
