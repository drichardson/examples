// Every Thread in sclang has a (pseudo-) random number generator that is responsible for all randomization within this thread. Each random number generator has its own seed (starting point) from which the series of values is generated. This seed can be set and after that, the randgen (being strictly deterministic) produces exactly the same numbers again.

// In order to save disk space, you can reproduce any sequence of randomized data just by one Integer number that you can write down in your notebook.

// every thread, also a Routine, has a random generator seed:
(
r = Routine({
    loop({#[1,2,3,4,5].choose.yield })
});
r.randSeed = 1923;
)

// using the routine to fill an array
Array.fill(7, r);

// setting the random generator seed back to our initial seed
r.randSeed = 1923;

// causes this array to be identical
Array.fill(7, r);



