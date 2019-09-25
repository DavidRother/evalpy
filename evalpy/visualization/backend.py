from ..project import client, sql_utilities
import os.path


class Backend:

    def __init__(self):
        self.client = client.Client()

    def load_database(self, directory):
        root = os.path.dirname(directory)
        name = os.path.basename(directory)
        self.client.set_project(root, name)
        return name

    def get_experiment_names(self):
        return [item[0] for item in set(self.client.get_stored_experiment_names())]

    def get_run_ids_by_experiment_names(self, experiment_names):
        return [item[0] for item in self.client.get_runs_by_experiment_names(experiment_names)]

    def get_column_names_of_experiments(self):
        return self.client.column_names_of_experiments()

    def get_column_names_of_run(self, run_id):
        return self.client.column_names_of_run(run_id)

    def get_filtered_column_values_experiments(self, experiment_names, sql_filters, columns):
        return self.client.filtered_column_values_experiment(experiment_names, sql_filters, columns)

    def get_filtered_column_values_run(self, run_id, sql_filters, columns):
        return self.client.filtered_column_values_run(run_id, sql_filters, columns)

    @staticmethod
    def get_filter_string_and_object(junction, entry, operator, value, start_bracket, end_bracket):
        filter_object = sql_utilities.sql_where_filter(junction, entry, operator, value, start_bracket, end_bracket)
        filter_string = sql_utilities.translate_sql_filter(filter_object)
        return filter_string, filter_object

