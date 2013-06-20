Demonstrates how using __weak without an autorelease pool can lead to leaks.

The problem is that using a weak ends up using objc_loadWeak which atomically retain/autoreleases the values so that you can use it in a thread
safe fashion. The problem is that (at least on 64-bit Mountain Lion) it doesn't warn that no autorelease pool is present and so you objects
are never released.
