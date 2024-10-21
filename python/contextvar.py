# Demonstrates that each awaited task has it's own copy of ContextVar's value.

from contextvars import ContextVar
import asyncio

myvar: ContextVar[float] = ContextVar('myvar', default=42)

print("myvar: ", myvar.get())
token = myvar.set(55)
print("myvar: ", myvar.get())
print("token: ", token)
myvar.reset(token)
print("token: ", token)
print("myvar: ", myvar.get())

async def runtask(i):
    print(f"main({i}): pre-sleep pre-set  myvar: ", myvar.get())
    myvar.set(i)
    print(f"main({i}): pre-sleep post-set myvar: ", myvar.get())
    await asyncio.sleep(i)
    print(f"main({i}): post-sleep         myvar: ", myvar.get())
    myvar.set(-i)

async def main(i):
    print(f"main{i} CALLED =====================")
    await runtask(1.5)
    await asyncio.gather(
        runtask(3),
        runtask(2),
        runtask(1)
    )
    print(f"main{i} ENDING =====================")

asyncio.run(main(1))
#asyncio.run(main(10))
