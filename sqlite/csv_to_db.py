#!/usr/bin/python
# Usage: csv_to_db.py <input_csv> <output_database>
# Convert a CSV database into a sqlite database.

import csv
import sqlite3
import sys

if len(sys.argv) != 3:
    print "Missing required argument"
    print "Usage: csv_to_db.py <input_csv> <output_database>"
    sys.exit(1)

inputCSVFile = sys.argv[1]
databaseFile = sys.argv[2]

conn = sqlite3.connect(databaseFile)
c = conn.cursor()

with open(inputCSVFile, 'rU') as csvfile:
    reader = csv.reader(csvfile, delimiter=',', quotechar='"')
    headerRow = next(reader, None)
    commaSeparatedFieldnames = ",".join(headerRow)
    commaSeparatedPlaceholders = ",".join(['?' for i in range(len(headerRow))])
    c.execute('create table data(%s)' % commaSeparatedFieldnames)
    insertStatement = 'insert into data(%s) values (%s)' % (commaSeparatedFieldnames, commaSeparatedPlaceholders)
    for row in reader:
        c.execute(insertStatement, row)

conn.commit()
conn.close()

print "OK"
