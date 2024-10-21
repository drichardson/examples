import pytest
import logging
import logging.config

logging.config.fileConfig("logging.conf")

class MyHandler(logging.Handler):
    def __init__(self):
        self.records = []
        logging.Handler.__init__(self)

    def emit(self, record):
        self.records.append(record)

def test_log_test():
    l = logging.getLogger('test_logger')
    l.setLevel(logging.INFO)
    l.info("info log 1")
    handler = MyHandler()
    l.addHandler(handler)
    assert len(handler.records) == 0
    
    l.info("info log 2")
    assert len(handler.records) == 1
    assert handler.records[0].message == "info log 2"
