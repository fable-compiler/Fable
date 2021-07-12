from fable.reflection import class_type

def lifted_1(b):
    return Math$(b)


def lifted_3():
    return class_type("QuickTest.Math", None, Math$)


Math$ = Lifted_2

Math$$reflection = lifted_3

Math_$ctor_Z524259A4 = lifted_1

