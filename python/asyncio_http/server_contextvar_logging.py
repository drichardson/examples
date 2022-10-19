from aiohttp import web
from contextvars import ContextVar
import asyncio
import logging


log_context_user_id: ContextVar[str] = ContextVar('user_id', default=None)
log_context_username: ContextVar[str] = ContextVar('username', default=None)
log_context_email: ContextVar[str] = ContextVar('email', default=None)

class ContextFilter(logging.Filter):
    def __init__(self):
        super(ContextFilter, self).__init__()

    def filter(self, record):
        record.user_id = log_context_user_id.get()
        record.username =  log_context_username.get()
        record.email = log_context_email.get()
        return True


logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
context_filter = ContextFilter()
logger.addFilter(context_filter)

format_string = "[id=%(user_id)s name=%(username)s email%=(email)s]: %(message)s"
stdout_formatter = logging.Formatter(format_string)
stdout_handler = logging.StreamHandler()
stdout_handler.setFormatter(stdout_formatter)
logger.addHandler(stdout_handler)

logger.info("STARTING")

async def handle(request):
    name = request.match_info.get("name", "Aynoymous")
    log_context_username.set(name)
    logger.error("handling request sleeping")
    await asyncio.sleep(5)
    logger.error("handling request sleeping woke up")
    name = request.match_info.get("name", "Aynoymous")
    return web.Response(text="Hello, " + name)

app = web.Application()
app.add_routes([web.get('/', handle),
                web.get('/{name}', handle)])


web.run_app(app)
