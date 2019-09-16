from typing import Dict
from collections import defaultdict

import sqlite3

type_translation = defaultdict(lambda: 'blob', {int: 'integer', str: 'text', float: 'real'})


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


def create_run_table(cursor, table_name):
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


def _correct_column_scheme(connection, params, table_name):
    cursor = connection.execute(f'select * from {table_name}')
    current_columns = [description[0] for description in cursor.description]
    columns_to_be_created = set(current_columns) - set(params.keys())
    for column_name in columns_to_be_created:
        sql_statement = f"ALTER TABLE params ADD COLUMN {column_name} {type_translation[type(params[column_name])]}"
        cursor.execute(sql_statement)


def _add_data_row_to_table(cursor, params, table_name):
    columns = ', '.join(params.keys())
    placeholders = ', '.join('?' * len(params))
    sql = 'INSERT INTO {} ({}) VALUES ({})'.format(table_name, columns, placeholders)
    cursor.execute(sql, params.values())
