import sys
from PyQt5 import QtWidgets
from evalpy.visualization.window import MyWindow


app = QtWidgets.QApplication(sys.argv)
window = MyWindow()
window.show()
sys.exit(app.exec_())
