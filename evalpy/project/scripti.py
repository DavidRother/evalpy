import sqlite3


conn = sqlite3.connect('example.db')

c = conn.cursor()

c.execute('''CREATE TABLE stocks2''')

conn.commit()

conn.close()
