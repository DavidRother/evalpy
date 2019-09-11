import sqlite3


def establish_connection(db_file: str):
    sqlite3.connect(db_file)
