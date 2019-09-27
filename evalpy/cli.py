import click
import sys
import os.path
from PyQt5 import QtWidgets
from evalpy.visualization.window import MyWindow


@click.group()
@click.version_option()
def cli():
    pass


@cli.command()
def run():
    my_path = os.path.realpath(__file__)
    dir_name = os.path.dirname(my_path)
    interface_file = os.path.join(dir_name, 'visualization/interface.ui')
    icon_path = os.path.join(dir_name, 'visualization/icons8-flip-chart-96.png')
    app = QtWidgets.QApplication(sys.argv)
    window = MyWindow(interface_file, icon_path)
    window.show()
    sys.exit(app.exec_())


if __name__ == '__main__':
    cli()
