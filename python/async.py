import asyncio

async def main():
    print('hello')
    await asyncio.sleep(1)
    print('world')


#print("main: ", main())
print("before")
asyncio.run(main())
print('after')

