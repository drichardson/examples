from collections import deque
from contextlib import contextmanager
import logging
import sys


class LoggingContextHandler:
    def __init__(self):
        self.attributes = deque([{}])

    def add(self, **new_context_vars):
        old_context = self.attributes[0]
        new_context = {**old_context, **new_context_vars}
        self.attributes.appendleft(new_context)

    def get(self, key):
        return self.attributes[0].get(key)

    def remove(self):
        self.attributes.popleft()

    def __str__(self):
        return str(self.attributes)


logging_context_handler = LoggingContextHandler()


@contextmanager
def logging_context(**kwargs):
    logging_context_handler.add(**kwargs)
    yield
    logging_context_handler.remove()


class ContextFilter(logging.Filter):
    def __init__(self):
        super(ContextFilter, self).__init__()

    def filter(self, record):
        record.store = logging_context_handler.get("store")
        record.client = logging_context_handler.get("client")
        record.item = logging_context_handler.get("item")

        return True


logger = logging.getLogger()
logger.setLevel(logging.INFO)
context_filter = ContextFilter()
logger.addFilter(context_filter)


format_string = "[%(store)s | %(client)s | %(item)s]: %(message)s"
stdout_formatter = logging.Formatter(format_string)
stdout_handler = logging.StreamHandler(sys.stdout)
stdout_handler.setFormatter(stdout_formatter)
logger.addHandler(stdout_handler)

clients = {"Jim": ["potatoes", "tomatoes"], "Tim": ["bread", "eggs", "milk"]}


def sell_goods(shopping_list):
    for item in shopping_list:
        with logging_context(item=item):
            logger.info("Sold 1 item.")

print("HI")

with logging_context(store="Hannah's Grocery Store"):
    for client, shopping_list in clients.items():
        with logging_context(client=client):
            sell_goods(shopping_list)


print("END")
