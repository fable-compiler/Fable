
def test():
    value = 2
    def lifted_0(_=None):
        nonlocal value
        value = 42
    
    fn = lifted_0
    fn()


