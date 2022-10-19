from aiohttp import web
import logging

logger = logging.getLogger()
logger.setLevel(logging.INFO)

async def handle(request):
    logger.error("handling request")
    name = request.match_info.get("name", "Aynoymous")
    return web.Response(text="Hello, " + name)

app = web.Application()
app.add_routes([web.get('/', handle),
                web.get('/{name}', handle)])


web.run_app(app)
