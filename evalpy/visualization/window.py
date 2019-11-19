from PyQt5 import QtCore, QtGui, QtWidgets, uic
from typing import List
from .backend import Backend
from ..project.sql_utilities import SQLOperator, SQLJunction
from ..project import sql_utilities
import pyqtgraph as pg
import pyqtgraph.exporters
import pandas as pd
import numpy as np


class MyWindow(QtWidgets.QMainWindow):

    def __init__(self, file_path='interface.ui', icon_path='icons8-flip-chart-96.png'):
        QtWidgets.QMainWindow.__init__(self)
        uic.loadUi(file_path, self)
        self.setWindowIcon(QtGui.QIcon(icon_path))
        self.backend = Backend()
        self.active_data = 'experiment'
        self.populate_combo_boxes()
        self.actionLoad_Project.triggered.connect(self.get_directory_dialog)
        self.actionExport_Data_Selection_as_csv.triggered.connect(self.export_data_selection_to_csv)
        self.actionDelete_Experiment.triggered.connect(self.delete_experiment)
        self.actionDelete_Run.triggered.connect(self.delete_run)
        self.list_widget_experiments.itemSelectionChanged.connect(self.experiment_selection_changed)
        self.list_widget_runs.itemSelectionChanged.connect(self.run_selection_changed)
        self.push_button_switch_data_view.clicked.connect(self.switch_run_experiment_data)
        self.push_button_add_filter.clicked.connect(self.add_filter)
        self.push_button_filter_remove.clicked.connect(self.remove_filter_items)
        self.push_button_compute_plot.clicked.connect(self.compute_plot)
        self.push_button_apply_filter_to_runs.clicked.connect(self.apply_filter_to_runs)
        self.push_button_export_plot.clicked.connect(self.export_plot)

    def switch_run_experiment_data(self):
        self.active_data = 'run' if self.active_data == 'experiment' else 'experiment'
        self.list_widget_filter.clear()
        self.combo_box_entry.clear()
        self.line_edit_filter_value.setText('')
        self.combo_box_x_axis.clear()
        self.combo_box_y_axis.clear()
        if self.active_data == 'run':
            self.run_selection_changed()
        else:
            self.experiment_selection_changed()

    def apply_filter_to_runs(self):
        if self.active_data == 'run':
            return
        filters = []
        for idx in range(self.list_widget_filter.count()):
            filters.append(self.list_widget_filter.item(idx).data(QtCore.Qt.UserRole))
        if not filters:
            return
        experiment_names = self.get_selected_experiment_names()
        run_ids = self.backend.get_filtered_column_values_experiments(experiment_names, filters, ['run_id'])
        new_run_ids = [item[0] for item in run_ids]
        self.list_widget_runs.clear()
        self.label_results_number.setText(str(len(new_run_ids)))
        self.list_widget_runs.addItems(new_run_ids)

    def get_directory_dialog(self):
        options = QtWidgets.QFileDialog.Options()
        options |= QtWidgets.QFileDialog.DontUseNativeDialog
        directory = str(QtWidgets.QFileDialog.getExistingDirectory(self, 'Select Directory', options=options))
        if not directory:
            return
        self.set_project_database(directory)

    def set_project_database(self, directory):
        name = self.backend.load_database(directory)
        self.label_project_name.setText(name)
        experiment_names = self.backend.get_experiment_names()
        self.list_widget_experiments.addItems(experiment_names)

    def experiment_selection_changed(self):
        self.list_widget_runs.clear()
        self.combo_box_entry.clear()
        self.combo_box_x_axis.clear()
        self.combo_box_y_axis.clear()
        self.line_edit_filter_value.setText('')
        experiment_names = self.get_selected_experiment_names()
        if not experiment_names:
            return
        new_run_ids = self.backend.get_run_ids_by_experiment_names(experiment_names)
        self.label_results_number.setText(str(len(new_run_ids)))
        self.list_widget_runs.addItems(new_run_ids)
        self.populate_entry_combo_box()
        self.populate_axis_boxes()

    def run_selection_changed(self):
        run_ids = self.get_selected_run_ids()
        if not run_ids:
            return
        self.populate_table(run_ids[0])
        if self.active_data == 'experiment':
            return
        self.combo_box_entry.clear()
        self.line_edit_filter_value.setText('')
        self.populate_entry_combo_box()
        self.populate_axis_boxes()

    def populate_table(self, run_id):
        column_names = self.backend.get_column_names_of_experiments()
        sql_filter = sql_utilities.sql_where_filter(SQLJunction.NONE, 'run_id', SQLOperator.EQUALS, run_id,
                                                    False, False)
        column_values = self.backend.get_filtered_column_values_experiments([], [sql_filter], column_names)
        self.table_widget_run_params.setRowCount(len(column_names))
        self.table_widget_run_params.setColumnCount(2)
        sorted_entries = sorted(zip(column_names, column_values[0]), key=lambda x: x[0])
        for row_idx, entry in enumerate(sorted_entries):
            self.table_widget_run_params.setItem(row_idx, 0, QtWidgets.QTableWidgetItem(str(entry[0])))
            try:
                self.table_widget_run_params.setItem(row_idx, 1, QtWidgets.QTableWidgetItem(f'{entry[1]:.2f}'))
            except (ValueError, TypeError):
                self.table_widget_run_params.setItem(row_idx, 1, QtWidgets.QTableWidgetItem(str(entry[1])))

    def add_filter(self):
        junction = self.combo_box_junction.currentData()
        entry = self.combo_box_entry.currentText()
        operator = self.combo_box_operator.currentData()
        value = self.line_edit_filter_value.text()
        start_bracket = self.radio_button_start_bracket.isChecked()
        end_bracket = self.radio_button_end_bracket.isChecked()
        if not value or not entry:
            return
        if self.list_widget_filter.count() == 0:
            junction = SQLJunction.NONE
        filter_string, filter_object = self.backend.get_filter_string_and_object(junction, entry, operator, value,
                                                                                 start_bracket, end_bracket)
        item = QtWidgets.QListWidgetItem(filter_string, self.list_widget_filter)
        item.setData(QtCore.Qt.UserRole, filter_object)

    def populate_combo_boxes(self):
        self.combo_box_junction.addItem('NONE', SQLJunction.NONE)
        self.combo_box_junction.addItem(SQLJunction.OR.value, SQLJunction.OR)
        self.combo_box_junction.addItem(SQLJunction.AND.value, SQLJunction.AND)

        self.combo_box_operator.addItem(SQLOperator.GREATER.value, SQLOperator.GREATER)
        self.combo_box_operator.addItem(SQLOperator.GREATER_EQUALS.value, SQLOperator.GREATER_EQUALS)
        self.combo_box_operator.addItem(SQLOperator.EQUALS.value, SQLOperator.EQUALS)
        self.combo_box_operator.addItem(SQLOperator.NOT_EQUALS.value, SQLOperator.NOT_EQUALS)
        self.combo_box_operator.addItem(SQLOperator.LESSER.value, SQLOperator.LESSER)
        self.combo_box_operator.addItem(SQLOperator.LESSER_EQUALS.value, SQLOperator.LESSER_EQUALS)

    def populate_entry_combo_box(self):
        self.combo_box_entry.clear()
        if self.active_data == 'experiment':
            column_names = self.backend.get_column_names_of_experiments()
        else:
            run_ids = self.get_selected_run_ids()
            column_names = [self.backend.get_column_names_of_run(run_id) for run_id in run_ids]
            if not column_names:
                return
            elif len(column_names) == 1:
                column_names = column_names[0]
            else:
                column_names = list(set(column_names[0]).intersection(*column_names[1:]))
        self.combo_box_entry.addItems(sorted(column_names))

    def get_selected_experiment_names(self) -> List[str]:
        return [str(item.text()) for item in self.list_widget_experiments.selectedItems()]

    def get_selected_run_ids(self) -> List[str]:
        return [str(item.text()) for item in self.list_widget_runs.selectedItems()]

    def remove_filter_items(self):
        for item in self.list_widget_filter.selectedItems():
            self.list_widget_filter.takeItem(self.list_widget_filter.row(item))
        if self.list_widget_filter.count() > 0:
            item = self.list_widget_filter.item(0)
            tmp = item.data(QtCore.Qt.UserRole)
            filter_string, filter_object = self.backend.get_filter_string_and_object(SQLJunction.NONE, tmp.entry,
                                                                                     tmp.operator, tmp.value,
                                                                                     tmp.start_bracket, tmp.end_bracket)
            item.setData(QtCore.Qt.UserRole, filter_object)
            item.setText(filter_string)

    def populate_axis_boxes(self):
        self.combo_box_x_axis.clear()
        self.combo_box_y_axis.clear()
        if self.active_data == 'experiment':
            column_names = self.backend.get_column_names_of_experiments()
        else:
            run_ids = self.get_selected_run_ids()
            column_names = [self.backend.get_column_names_of_run(run_id) for run_id in run_ids]
            if not column_names:
                return
            elif len(column_names) == 1:
                column_names = column_names[0]
            else:
                column_names = list(set(column_names[0]).intersection(*column_names[1:]))
        self.combo_box_x_axis.addItems(sorted(column_names))
        self.combo_box_y_axis.addItems(sorted(column_names))

    def compute_plot(self):
        if self.active_data == 'experiment':
            self.compute_experiment_plot()
        else:
            self.compute_run_plot()

    def compute_experiment_plot(self):
        x_axis = self.combo_box_x_axis.currentText()
        y_axis = self.combo_box_y_axis.currentText()
        if not x_axis or not y_axis:
            return
        filters = []
        for idx in range(self.list_widget_filter.count()):
            filters.append(self.list_widget_filter.item(idx).data(QtCore.Qt.UserRole))
        experiment_names = self.get_selected_experiment_names()
        values = self.backend.get_filtered_column_values_experiments(experiment_names, filters, [x_axis, y_axis])
        values = [item for item in values if None not in item]
        if not values:
            return
        x_values, y_values = zip(*values)
        if x_values and y_values:
            self._prepare_plot_experiment(x_values, y_values, x_axis, y_axis)

    def compute_run_plot(self):
        x_axis = self.combo_box_x_axis.currentText()
        y_axis = self.combo_box_y_axis.currentText()
        if not x_axis or not y_axis:
            return
        filters = []
        for idx in range(self.list_widget_filter.count()):
            filters.append(self.list_widget_filter.item(idx).data(QtCore.Qt.UserRole))
        run_ids = self.get_selected_run_ids()
        if not run_ids:
            return

        run_values_list = [self.backend.get_filtered_column_values_run(run_id, filters, [x_axis, y_axis])
                           for run_id in run_ids]
        run_values_list = [[item for item in run_values if None not in item] for run_values in run_values_list]
        if run_values_list == [[]]:
            return
        self._prepare_plot_runs(run_values_list, x_axis, y_axis)

    def export_plot(self):
        # , filter='svg(*.svg)'
        options = QtWidgets.QFileDialog.Options()
        options |= QtWidgets.QFileDialog.DontUseNativeDialog
        file_name, _ = QtWidgets.QFileDialog.getSaveFileName(self, 'Select File name',
                                                             filter='Image Files (*.svg *.png)', options=options)
        if not file_name or file_name == '(\'\', \'\')':
            return
        if not (file_name.endswith('.svg') or file_name.endswith('.png')):
            file_name += '.svg'
        if file_name.endswith('.png'):
            plot_item = self.plot_widget.getPlotItem()
            exporter = pg.exporters.ImageExporter(plot_item)
            self._fix_pyqtgraph_image_exporter_bug(exporter)
        else:
            plot_item = self.plot_widget.getPlotItem()
            exporter = pg.exporters.SVGExporter(plot_item)
        exporter.export(file_name)

    def export_data_selection_to_csv(self):
        if self.active_data == 'experiment':
            self.export_experiment_data_to_csv()
        else:
            self.export_run_data_to_csv()

    def export_experiment_data_to_csv(self):
        filters = []
        for idx in range(self.list_widget_filter.count()):
            filters.append(self.list_widget_filter.item(idx).data(QtCore.Qt.UserRole))
        experiment_names = self.get_selected_experiment_names()
        column_names = self.backend.get_column_names_of_experiments()
        values = self.backend.get_filtered_column_values_experiments(experiment_names, filters, column_names)
        for idx, row in enumerate(values):
            values[idx] = [str(value) for value in row]
        options = QtWidgets.QFileDialog.Options()
        options |= QtWidgets.QFileDialog.DontUseNativeDialog
        file_name, _ = QtWidgets.QFileDialog.getSaveFileName(self, 'Select File name',
                                                             filter='CSV Files (*.csv)', options=options)
        if not file_name or file_name == '(\'\', \'\')':
            return
        if not file_name.endswith('.csv'):
            file_name += '.csv'
        df = pd.DataFrame(values, columns=column_names)
        df.to_csv(file_name)

    def export_run_data_to_csv(self):
        filters = []
        for idx in range(self.list_widget_filter.count()):
            filters.append(self.list_widget_filter.item(idx).data(QtCore.Qt.UserRole))
        run_ids = self.get_selected_run_ids()
        if not run_ids:
            return
        run_ids = self.get_selected_run_ids()
        column_names = [self.backend.get_column_names_of_run(run_id) for run_id in run_ids]
        run_values_list = [self.backend.get_filtered_column_values_run(run_id, filters, column_names[0])
                           for run_id in run_ids]
        data = run_values_list[0]
        options = QtWidgets.QFileDialog.Options()
        options |= QtWidgets.QFileDialog.DontUseNativeDialog
        file_name, _ = QtWidgets.QFileDialog.getSaveFileName(self, 'Select File name',
                                                             filter='CSV Files (*.csv)', options=options)
        if not file_name or file_name == '(\'\', \'\')':
            return
        if not file_name.endswith('.csv'):
            file_name += '.csv'
        df = pd.DataFrame(data, columns=column_names[0])
        df.to_csv(file_name)

    def delete_experiment(self):
        experiment_name, ok_pressed = QtWidgets.QInputDialog.getText(self, 'evalpy - Delete Experiment',
                                                                     'Experiment name', QtWidgets.QLineEdit.Normal, "")
        if ok_pressed:
            self.backend.delete_experiment(experiment_name)

    def delete_run(self):
        run_id, ok_pressed = QtWidgets.QInputDialog.getText(self, 'evalpy - Delete Run', 'Run ID',
                                                            QtWidgets.QLineEdit.Normal, "")
        if ok_pressed:
            self.backend.delete_run(run_id)

    def _prepare_plot_experiment(self, x_values, y_values, x_axis_name, y_axis_name):
        plot_widget: pg.PlotWidget = self.plot_widget
        plot_widget.clear()
        scatter = pg.ScatterPlotItem(pen=pg.mkPen(width=5, color='r'), symbol='o', size=1)
        transformed_x = self._transform_data_and_axis(plot_widget, x_values, 'bottom', x_axis_name)
        transformed_y = self._transform_data_and_axis(plot_widget, y_values, 'left', y_axis_name)
        plot_widget.addItem(scatter)
        try:
            scatter.setData(transformed_x, transformed_y)
        except TypeError as e:
            print(e)

    def _transform_data_and_axis(self, plot_widget, values, axis_side, axis_name):
        if isinstance(values[0], str):
            return self._prepare_string_axis(plot_widget, axis_side, values, axis_name)
        else:
            return self._prepare_number_axis(plot_widget, axis_side, values, axis_name)

    def _prepare_plot_runs(self, run_values_list, x_axis_name, y_axis_name):
        plot_widget: pg.PlotWidget = self.plot_widget
        plot_widget.clear()
        self._set_axis_properties(plot_widget, 'bottom', x_axis_name, None)
        self._set_axis_properties(plot_widget, 'left', y_axis_name, None)
        for idx, run_values in enumerate(run_values_list):
            x_values, y_values = zip(*run_values)
            series = pg.PlotCurveItem(pen=pg.mkPen(width=1, color=pg.intColor(idx)))
            plot_widget.addItem(series)
            series.setData(list(x_values), list(y_values))

    def _prepare_string_axis(self, plot_widget, axis_side, values, axis_name):
        tick_dict = {val: idx for idx, val in enumerate(set(values))}
        transformed_values = [tick_dict[item] for item in values]
        ticks = [list(enumerate(set(values)))]
        self._set_axis_properties(plot_widget, axis_side, axis_name, ticks)
        return transformed_values

    def _prepare_number_axis(self, plot_widget, axis_side, values, axis_name):
        self._set_axis_properties(plot_widget, axis_side, axis_name, None)
        return values

    @staticmethod
    def _set_axis_properties(plot_widget, axis_side, axis_name, ticks=None):
        axis = plot_widget.getAxis(axis_side)
        axis.setTicks(ticks)
        axis.setLabel(axis_name)

    @staticmethod
    def _fix_pyqtgraph_image_exporter_bug(exporter):
        # For why this dance is needed see:
        # https://github.com/pyqtgraph/pyqtgraph/issues/538
        width = exporter.parameters()['width']
        height = exporter.parameters()['height']
        width = int(width)
        height = int(height)
        exporter.params.param('width').setValue(width + 1, blockSignal=exporter.widthChanged)
        exporter.params.param('height').setValue(height + 1, blockSignal=exporter.heightChanged)
        exporter.params.param('width').setValue(width, blockSignal=exporter.widthChanged)
        exporter.params.param('height').setValue(height, blockSignal=exporter.heightChanged)

