import sys
from PyQt5 import QtCore, QtGui, QtWidgets, uic


qt_creator_file = "interface.ui"  # Enter file here.
Ui_MainWindow, QtBaseClass = uic.loadUiType(qt_creator_file)


def hello():
    print('hi')


def get_directory(parent):
    print('hi')
    directory = str(QtWidgets.QFileDialog.getExistingDirectory(parent, 'Select Directory'))
    print(directory)


class MyWindow(QtWidgets.QMainWindow, Ui_MainWindow):

    def __init__(self):
        QtWidgets.QMainWindow.__init__(self)
        Ui_MainWindow.__init__(self)
        self.setupUi(self)
        self.push_button_switch_to_run_view.clicked.connect(hello)
        self.actionLoad_Project.triggered.connect(lambda: get_directory(self))


if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = MyWindow()
    window.show()
    sys.exit(app.exec_())
