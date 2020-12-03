module Subgroups

"""
Generate a sub-group of the multiplicative group of integers modulo p
from powers of x.
"""
function generate(x::Integer, p::Integer)
    Set(BigInt[x^i%p for i=1:p])
end

end
