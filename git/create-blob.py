#!/usr/bin/python3

# Example from https://git-scm.com/book/en/v2/Git-Internals-Git-Objects,
# translated to Python 3.

import hashlib
import pathlib
import sys
import zlib

content = 'what is up, doc?'
header = f'blob {len(content)}\0'
store = (header + content).encode('utf-8')

sha1=hashlib.sha1()
sha1.update(store)
digest = sha1.digest().hex()

zlib_content = zlib.compress(store)

if not pathlib.Path('.git').exists():
    print('This command must be run at the root of a git directory. Run "git init" and try again.')
    sys.exit(1)

path = pathlib.Path('.git/objects').joinpath(digest[0:2], digest[2:])
path.parent.mkdir(parents=True, exist_ok=True)
path.write_bytes(zlib_content)

print(f'View object with:\n\tgit cat-file -p {digest}')
