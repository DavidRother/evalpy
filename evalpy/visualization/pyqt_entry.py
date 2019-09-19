import sys
from PyQt5 import QtCore, QtGui, QtWidgets, uic


qt_creator_file = "interface.ui"  # Enter file here.
Ui_MainWindow, QtBaseClass = uic.loadUiType(qt_creator_file)


class MyWindow(QtWidgets.QMainWindow, Ui_MainWindow):
    def __init__(self):
        QtWidgets.QMainWindow.__init__(self)
        Ui_MainWindow.__init__(self)
        self.setupUi(self)


if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = MyWindow()
    window.show()
    sys.exit(app.exec_())
