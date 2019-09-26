from . import sql_utilities
from typing import Dict, List

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
        self.run_entry_dict.update(entry_dict)

    def forward_step(self):
        self.run_step_entry_dicts.append({})

    def log_step(self, entry_dict: Dict, step_forward=False):
        self.run_step_entry_dicts[-1].update(entry_dict)
        if step_forward:
            self.forward_step()

    def get_stored_experiment_names(self):
        return [item for item in set(sql_utilities.get_column_values(self.db_connection, 'experiment_name'))]

    def get_all_stored_run_ids(self):
        return [item for item in set(sql_utilities.get_column_values(self.db_connection, 'run_id'))]

    def get_runs_by_experiment_names(self, experiment_names):
        where_filters = [sql_utilities.sql_where_filter(sql_utilities.SQLJunction.NONE, 'experiment_name',
                                                        sql_utilities.SQLOperator.EQUALS, experiment_names[0], False,
                                                        False)]

        where_filters.extend([sql_utilities.sql_where_filter(sql_utilities.SQLJunction.OR, 'experiment_name',
                                                             sql_utilities.SQLOperator.EQUALS, name, False, False)
                              for name in experiment_names[1:]])
        return sql_utilities.get_column_values_filtered(self.db_connection, ['run_id'], 'params', where_filters)

    def filtered_column_values_experiment(self, experiment_names, sql_filters: List[sql_utilities.sql_where_filter],
                                          columns: List[str]):
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
