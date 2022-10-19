from aiohttp import web
import asyncio
import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

logger.info("STARTING")

async def handle(request):
    logger.error("handling request sleeping")
    await asyncio.sleep(5)
    logger.error("handling request sleeping woke up")
    name = request.match_info.get("name", "Aynoymous")
    return web.Response(text="Hello, " + name)

app = web.Application()
app.add_routes([web.get('/', handle),
                web.get('/{name}', handle)])


web.run_app(app)
