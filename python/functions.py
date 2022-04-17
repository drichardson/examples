def standard_arg(arg):
    print(arg)

def pos_only_arg(arg, /):
    print(arg)

def kwd_only_arg(*, arg):
    print(arg)

def combined_example(pos_only, /, standard, *, kwd_only):
    print(pos_only, standard, kwd_only)

def star_args(*argv):
    print(argv)

def star_kwargs(**kwargs):
    print(kwargs)


standard_arg(1)
standard_arg(arg=123)

pos_only_arg(123)
#pos_only_arg(arg=123)

#kwd_only_arg(123)
#kwd_only_arg(arg2=123)
kwd_only_arg(arg=444)

combined_example(1, 2, kwd_only=3)
combined_example(1, standard=2, kwd_only=3)
#combined_example(pos_only=1, standard=2, kwd_only=3)

star_args(1,2,3)
star_args(1,2)
# star_args(1,2, two="hi")

#star_kwargs(1,2,3)
star_kwargs(one=1,two=2,three=3)
