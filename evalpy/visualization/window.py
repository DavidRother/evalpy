from PyQt5 import QtCore, QtGui, QtWidgets, uic
from PyQt5.QtChart import *
from typing import List
from .backend import Backend
from ..project.sql_utilities import SQLOperator, SQLJunction
import numpy as np


qt_creator_file = "interface.ui"  # Enter file here.
Ui_MainWindow, QtBaseClass = uic.loadUiType(qt_creator_file)


class MyWindow(QtWidgets.QMainWindow, Ui_MainWindow):

    def __init__(self):
        QtWidgets.QMainWindow.__init__(self)
        Ui_MainWindow.__init__(self)
        self.setWindowIcon(QtGui.QIcon('icons8-flip-chart-96.png'))
        self.setupUi(self)
        self.backend = Backend()
        self.active_data = 'experiment'
        self.progress_bar.setValue(0)
        self.populate_combo_boxes()
        self.actionLoad_Project.triggered.connect(self.get_directory_dialog)
        self.list_widget_experiments.itemSelectionChanged.connect(self.experiment_selection_changed)
        self.list_widget_runs.itemSelectionChanged.connect(self.run_selection_changed)
        self.push_button_switch_data_view.clicked.connect(self.switch_run_experiment_data)
        self.push_button_add_filter.clicked.connect(self.add_filter)
        self.push_button_filter_remove.clicked.connect(self.remove_filter_items)
        self.push_button_compute_plot.clicked.connect(self.compute_plot)
        self.push_button_apply_filter_to_runs.clicked.connect(self.apply_filter_to_runs)
        self.progress_bar.setValue(100)

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
        directory = str(QtWidgets.QFileDialog.getExistingDirectory(self, 'Select Directory'))
        if not directory:
            return
        name = self.backend.load_database(directory)
        self.label_project_name.setText(name)
        experiment_names = self.backend.get_experiment_names()
        self.list_widget_experiments.addItems(experiment_names)

    def experiment_selection_changed(self):
        self.list_widget_runs.clear()
        self.combo_box_entry.clear()
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
        if self.active_data == 'experiment':
            return
        self.combo_box_entry.clear()
        self.line_edit_filter_value.setText('')
        run_ids = self.get_selected_run_ids()
        if not run_ids:
            return
        self.populate_entry_combo_box()
        self.populate_axis_boxes()

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
            column_names = list(set().union(*column_names))
        self.combo_box_entry.addItems(column_names)

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
            self.combo_box_x_axis.addItems(column_names)
        else:
            run_ids = self.get_selected_run_ids()
            column_names = [self.backend.get_column_names_of_run(run_id) for run_id in run_ids]
            if not column_names:
                return
            column_names = list(set().union(*column_names))
            self.combo_box_x_axis.addItem('step')
        self.combo_box_y_axis.addItems(column_names)

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
        self._prepare_plot(values, x_axis, y_axis)

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
        values = self.backend.get_filtered_column_values_run(run_ids[0], filters, [x_axis, y_axis])
        self._prepare_run_plot(values, x_axis, y_axis)

    def _prepare_plot(self, values, x_axis_name, y_axis_name):
        # https://stackoverflow.com/questions/51338580/pyqtchart-not-displaying-data
        a: QChartView = self.chart_view
        chart = QChart(flags=a.windowFlags())
        series = QScatterSeries()
        series.setMarkerSize(10.0)
        array_values = np.asarray(values)
        x_min = min(array_values[:, 0])
        x_max = max(array_values[:, 0])
        y_min = min(array_values[:, 1])
        y_max = max(array_values[:, 1])
        for x, y in values:
            if not x or not y:
                continue
            series.append(QtCore.QPointF(x, y))
        chart.addSeries(series)
        chart.setAnimationOptions(QChart.SeriesAnimations)

        # X Axis Settings
        axis_x = QValueAxis()
        axis_x.setLabelFormat("%.2f")
        axis_x.setTitleText(x_axis_name)
        chart.addAxis(axis_x, QtCore.Qt.AlignBottom)
        series.attachAxis(axis_x)

        # Y Axis Settings
        axis_y = QValueAxis()
        axis_y.setLabelFormat("%.2f")
        axis_y.setTitleText(y_axis_name)
        chart.addAxis(axis_y, QtCore.Qt.AlignLeft)
        series.attachAxis(axis_y)
        scale_factor = 0.3
        chart.axisX(series).setRange(x_min - scale_factor * np.abs(x_min - x_max),
                                     x_max + scale_factor * np.abs(x_min - x_max))
        chart.axisY(series).setRange(y_min - scale_factor * np.abs(y_min - y_max),
                                     y_max + scale_factor * np.abs(y_min - y_max))
        chart.legend().setVisible(False)
        a.setChart(chart)

    def _prepare_run_plot(self, values, x_axis_name, y_axis_name):
        # https://stackoverflow.com/questions/51338580/pyqtchart-not-displaying-data
        a: QChartView = self.chart_view
        chart = QChart(flags=a.windowFlags())
        series = QLineSeries()
        array_values = np.asarray(values)
        x_min = min(array_values[:, 0])
        x_max = max(array_values[:, 0])
        y_min = min(array_values[:, 1])
        y_max = max(array_values[:, 1])
        for x, y in values:
            if not x or not y:
                continue
            series.append(x, y)
        chart.addSeries(series)
        chart.setAnimationOptions(QChart.SeriesAnimations)

        # X Axis Settings
        axis_x = QValueAxis()
        axis_x.setLabelFormat("%.2f")
        axis_x.setTitleText(x_axis_name)
        chart.addAxis(axis_x, QtCore.Qt.AlignBottom)
        series.attachAxis(axis_x)

        # Y Axis Settings
        axis_y = QValueAxis()
        axis_y.setLabelFormat("%.2f")
        axis_y.setTitleText(y_axis_name)
        chart.addAxis(axis_y, QtCore.Qt.AlignLeft)
        series.attachAxis(axis_y)
        scale_factor = 0.3
        chart.axisX(series).setRange(x_min - scale_factor * np.abs(x_min - x_max),
                                     x_max + scale_factor * np.abs(x_min - x_max))
        chart.axisY(series).setRange(y_min - scale_factor * np.abs(y_min - y_max),
                                     y_max + scale_factor * np.abs(y_min - y_max))
        a.setChart(chart)
