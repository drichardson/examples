module Doug

" Compute Shannon entropy from a probability distribution "
function entropy(dist)
    -reduce(+, 0, map(xlog2x, dist))
end

function xlog2x(x)
    x * log2(x)
end

# using Gadfly
# Plot the entropy in the case of two possibilities with probabilities p and (1-p)
# from mathematical theory of communication page 50.
# plot([p -> Doug.entropy([p, 1-p])], 0.0, 1.0)

end

