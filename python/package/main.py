import f1
import p1.f2
import p1.p2.p3.f3
import p1.p2.intra.f4mod


print("main")
print("p1.p2 is ", type(p1.p2))
f1.func()
p1.f2.func()
p1.p2.p3.f3.func()
p1.f2.p2()

print("f1 is ", type(f1))
print("p1 is ", type(p1))
print("p1.intra.f4mod.f4 is", type(p1.p2.intra.f4mod.f4))

p1.p2.intra.f4mod.f4()
