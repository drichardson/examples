#!/bin/bash

javac -cp lucene-core-3.3.0.jar HelloLucene.java || exit 1
java -cp ".:lucene-core-3.3.0.jar" HelloLucene $1