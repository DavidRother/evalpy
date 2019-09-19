from . import sql_utilities
from typing import Dict

import os.path
import uuid


class Client:

    def __init__(self):
        self.project_root = None
        self.project_name = None
        self.experiment_name = 'default'
        self.database_file = 'database.db'
        self.db_connection = None
        self.cursor = None
        self.active_run_id = None
        self.run_entry_dict = None
        self.run_step_entry_dicts = None

    def __del__(self):
        # noinspection PyBroadException
        try:
            self.db_connection.close()
        except:
            pass

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.end_run()

    def __enter__(self):
        pass

    def set_project(self, root: str, name: str):
        self.project_root = root
        self.project_name = name
        os.makedirs(os.path.join(root, name), exist_ok=True)
        self.db_connection = sql_utilities.establish_connection(os.path.join(root, name, self.database_file))
        self.cursor = self.db_connection.cursor()

    def start_run(self, experiment_name=None):
        if self.active_run_id is not None:
            raise PermissionError('There is already an active run ongoing. '
                                  'Please finish it before starting another run.')
        self.experiment_name = experiment_name or self.experiment_name or 'default'
        self.active_run_id = 't' + str(uuid.uuid4()).replace('-', '_')
        sql_utilities.create_run_table(self.cursor, self.active_run_id)
        self.run_entry_dict = {}
        self.run_step_entry_dicts = [{}]
        return self

    def end_run(self):
        sql_utilities.add_row_to_main_table(self.db_connection, self.run_entry_dict)
        for entry in self.run_step_entry_dicts:
            sql_utilities.add_row_to_run_table(self.db_connection, self.active_run_id, entry)
        self.db_connection.commit()
        self.active_run_id = None
        self.run_entry_dict = None
        self.run_step_entry_dicts = None

    def log_entries(self, entry_dict: Dict):
        self.run_entry_dict.update(entry_dict)

    def forward_step(self):
        self.run_step_entry_dicts.append({})

    def log_step(self, entry_dict: Dict):
        self.run_step_entry_dicts[-1].update(entry_dict)
