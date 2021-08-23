luaunit = require('luaunit')
require('TestArithmetic')
TestMod = {}
function TestMod.testHello()
    assertEquals(1, 1)
end

luaunit.run()
