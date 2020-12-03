# Testing

#= This is a test
and stuff
=#

str = "teσst\u2022ing"
i = start(str)
while i <= endof(str)
    println(i)
    _, i = next(str, i)
    println("_ is $(_)")
end


