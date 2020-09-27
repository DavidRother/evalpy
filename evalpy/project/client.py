from . import sql_utilities
from typing import Dict, List, Optional

import os.path
import uuid
import pickle


class Client:

    def __init__(self):
        self.project_root = None
        self.project_name = None
        self.experiment_name = 'default'
        self.database_file = 'database.db'
        self.db_connection = None
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
        """
        Set the path where the database,db for the project can be found.
        All directories not yet existing will be automatically created.
        Connects the client to the database and creates one if none exists.

        :param root: The root path to the folder containing projects
        :param name: The project name and directory name where your folder with the database.db file is to be found
        """
        self.project_root = root
        self.project_name = name
        os.makedirs(os.path.join(root, name), exist_ok=True)
        self.db_connection = sql_utilities.establish_connection(os.path.join(root, name, self.database_file))

    def start_run(self, experiment_name: Optional[str] = None):
        """
        Starts a run session and enables the logging functions
        If used in a with construct this will automatically call end_run()
        else end_run() has to be called explicitly to commit changes to the database

        The intended use is with start_run(experiment_name='my_experiment'):

        :param experiment_name: The name of the experiment
        :return: The Client object
        """
        if self.active_run_id is not None:
            raise Exception('There is already an active run ongoing. Please finish it before starting another run.')
        self.experiment_name = experiment_name or self.experiment_name or 'default'
        self.active_run_id = 't' + str(uuid.uuid4()).replace('-', '_')
        sql_utilities.create_run_table(self.db_connection, self.active_run_id)
        self.run_entry_dict = {}
        self.run_step_entry_dicts = [{}]
        return self

    def end_run(self):
        """
        Parses and commits the logged entries of the current run

        """
        final_entry_dict = {'run_id': self.active_run_id, 'experiment_name': self.experiment_name,
                            **self.run_entry_dict}
        sql_utilities.add_row_to_main_table(self.db_connection, final_entry_dict)
        for entry in self.run_step_entry_dicts[:-1]:
            sql_utilities.add_row_to_run_table(self.db_connection, self.active_run_id, entry)
        self.db_connection.commit()
        self.active_run_id = None
        self.run_entry_dict = None
        self.run_step_entry_dicts = None

    def log_entries(self, entry_dict: Dict):
        """
        Logs a dictionary to the current run.
        Dictionary keys will be used as column names and should not contain SQL Keywords.

        :param entry_dict: A dictionary of values that should be recorded once per run
        """
        if not isinstance(entry_dict, dict):
            raise TypeError('Expected a Dictionary to add entries to the current run')
        if self.active_run_id is None:
            raise Exception("No active run ongoing")
        converted_dict = self._convert_dict(entry_dict)
        self.run_entry_dict.update(converted_dict)

    def forward_step(self):
        """
        Close the current step and start a new one for the continuous logged variables of the currently active run

        """
        if self.active_run_id is None:
            raise Exception("No active run ongoing")
        self.run_step_entry_dicts.append({})

    def log_step(self, entry_dict: Dict, step_forward=False):
        """
        Add a dictionary of entries to the current ongoing step to log the progression during a run

        :param entry_dict: A dictionary containing new entries
        :param step_forward: If True the method forward_step() will be called
        """
        if not isinstance(entry_dict, dict):
            raise TypeError('Expected a Dictionary to add entries to the current run')
        if self.active_run_id is None:
            raise Exception("No active run ongoing")
        converted_dict = self._convert_dict(entry_dict)
        self.run_step_entry_dicts[-1].update(converted_dict)
        if step_forward:
            self.forward_step()

    def commit_current_run_progress(self):
        """
        Commit all current entries in the run to the database

        """
        if self.active_run_id is None:
            raise Exception("No active run ongoing")
        for entry in self.run_step_entry_dicts[:-1]:
            sql_utilities.add_row_to_run_table(self.db_connection, self.active_run_id, entry)
        self.db_connection.commit()
        self.run_step_entry_dicts = [{}]

    def get_run_entries(self, run_id, columns: Optional[List[str]] = None):
        columns = columns or self.column_names_of_experiments()
        where_filter = sql_utilities.sql_where_filter(sql_utilities.SQLJunction.NONE, 'run_id',
                                                      sql_utilities.SQLOperator.EQUALS, run_id, False, False)
        values = sql_utilities.get_column_values_filtered(self.db_connection, columns, 'params', [where_filter])
        return values

    def delete_experiment(self, experiment_name: str):
        run_ids = self.get_runs_by_experiment_names([experiment_name])
        where_filter = sql_utilities.sql_where_filter(sql_utilities.SQLJunction.NONE, 'experiment_name',
                                                      sql_utilities.SQLOperator.EQUALS, experiment_name, False, False)
        sql_utilities.delete_rows(self.db_connection, 'params', [where_filter])
        for run_id in run_ids:
            sql_utilities.delete_table(self.db_connection, run_id[0])
        self.db_connection.commit()

    def delete_run(self, run_id):
        where_filter = sql_utilities.sql_where_filter(sql_utilities.SQLJunction.NONE, 'run_id',
                                                      sql_utilities.SQLOperator.EQUALS, run_id, False, False)
        sql_utilities.delete_rows(self.db_connection, 'params', [where_filter])
        sql_utilities.delete_table(self.db_connection, run_id)
        self.db_connection.commit()

    def get_stored_experiment_names(self):
        return [item for item in set(sql_utilities.get_column_values(self.db_connection, 'experiment_name'))]

    def get_all_stored_run_ids(self):
        return [item for item in set(sql_utilities.get_column_values(self.db_connection, 'run_id'))]

    def get_runs_by_experiment_names(self, experiment_names: List[str]):
        where_filters = [sql_utilities.sql_where_filter(sql_utilities.SQLJunction.NONE, 'experiment_name',
                                                        sql_utilities.SQLOperator.EQUALS, experiment_names[0], False,
                                                        False)]

        where_filters.extend([sql_utilities.sql_where_filter(sql_utilities.SQLJunction.OR, 'experiment_name',
                                                             sql_utilities.SQLOperator.EQUALS, name, False, False)
                              for name in experiment_names[1:]])
        return sql_utilities.get_column_values_filtered(self.db_connection, ['run_id'], 'params', where_filters)

    def filtered_column_values_experiment(self, experiment_names: List[str],
                                          sql_filters: List[sql_utilities.sql_where_filter], columns: List[str]):
        if sql_filters:
            name_filter = []
            for idx, name in enumerate(experiment_names):
                junction = sql_utilities.SQLJunction.AND if idx == 0 else sql_utilities.SQLJunction.OR
                name_filter.append(sql_utilities.sql_where_filter(junction, 'experiment_name',
                                                                  sql_utilities.SQLOperator.EQUALS, name, idx == 0,
                                                                  idx == (len(experiment_names)-1)))
        else:
            name_filter = [sql_utilities.sql_where_filter(sql_utilities.SQLJunction.NONE, 'experiment_name',
                                                          sql_utilities.SQLOperator.EQUALS, experiment_names[0], False,
                                                          False)]

            name_filter.extend([sql_utilities.sql_where_filter(sql_utilities.SQLJunction.OR, 'experiment_name',
                                                               sql_utilities.SQLOperator.EQUALS, name, False, False)
                                for name in experiment_names[1:]])
        sql_filters.extend(name_filter)
        return sql_utilities.get_column_values_filtered(self.db_connection, columns, 'params', sql_filters)

    def filtered_column_values_run(self, run_id, sql_filters: List[sql_utilities.sql_where_filter], columns: List[str]):
        return sql_utilities.get_column_values_filtered(self.db_connection, columns, run_id, sql_filters)

    def column_names_of_experiments(self):
        return sql_utilities.get_current_columns_of_table(self.db_connection, 'params')

    def column_names_of_run(self, run_id):
        return sql_utilities.get_current_columns_of_table(self.db_connection, run_id)

    @staticmethod
    def _convert_dict(table_entries: Dict):
        for key, value in table_entries.items():
            if sql_utilities.type_translation[type(value)] == 'blob':
                table_entries[key] = pickle.dumps(value)
        return table_entries

    def postprocess_db_output(self, values):
        pass
