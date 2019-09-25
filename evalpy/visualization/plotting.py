from PyQt5 import QtCore, QtGui, QtWidgets, uic
from PyQt5.QtChart import *


def create_x_axis(axis_type='category', axis_name=''):
    axis = None
    if axis_type == 'category':
        axis = create_category_axis()
    # axis


def create_category_axis():
    axis = QCategoryAxis()
    axis.setLabelsPosition(QCategoryAxis.AxisLabelsPositionOnValue)
    return axis
