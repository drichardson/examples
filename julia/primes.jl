module Primes

function randprime(bits)
    for i in 1:100_000
        x=rand(BigInt(1):BigInt(2)^bits-1)
        if isprime(x)
            return (true, x, i)
        end
    end
    return (false, 0, 0)
end

end

