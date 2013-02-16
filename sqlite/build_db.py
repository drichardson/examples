#!/usr/bin/python

import sqlite3
import uuid

def createdb(conn):
    c = conn.cursor()
    c.execute("CREATE TABLE employee (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL)")
    c.execute("CREATE INDEX employee_name on employee(name)")

def insertData(conn, rows):
    c = conn.cursor()
    for i in range(rows):
        c.execute("INSERT INTO employee (name) VALUES (?)", (str(uuid.uuid4()),))
    conn.commit()

def main():
    conn = sqlite3.connect('test.db')
    createdb(conn)
    insertData(conn, 25000)
    
    
    
main()
