luaunit = require('luaunit')

TestMod = {}
function TestMod.testHello()
    assertEquals(1, 1)
end

TestArithmetic = require('TestArithmetic')
TestRecords = require('TestRecords')

luaunit.run()
