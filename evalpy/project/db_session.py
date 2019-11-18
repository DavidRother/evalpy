from . import sql_utilities
from typing import Dict, List, Optional

import os.path
import uuid
import pickle


class DBSession:

    def __init__(self):
        self.project_root = None
        self.project_name = None
        self.database_file = 'database.db'
        self.db_connection = None

    def __del__(self):
        # noinspection PyBroadException
        try:
            self.db_connection.close()
        except:
            pass

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.db_connection.close()

    def __enter__(self, project_root: str, project_name: str):
        self.project_name = project_name
        self.project_root = project_root
        os.makedirs(os.path.join(self.project_root, self.project_name), exist_ok=True)
        self.db_connection = sql_utilities.establish_connection(os.path.join(self.project_root, self.project_name,
                                                                             self.database_file))
