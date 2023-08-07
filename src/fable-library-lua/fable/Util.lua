-- mod = {}

-- -- https://web.archive.org/web/20131225070434/http://snippets.luacode.org/snippets/Deep_Comparison_of_Two_Values_3
-- function deepcompare(t1,t2,ignore_mt)
--     local ty1 = type(t1)
--     local ty2 = type(t2)
--     if ty1 ~= ty2 then return false end
--     -- non-table types can be directly compared
--     if ty1 ~= 'table' and ty2 ~= 'table' then return t1 == t2 end
--     -- as well as tables which have the metamethod __eq
--     local mt = getmetatable(t1)
--     if not ignore_mt and mt and mt.__eq then return t1 == t2 end
--     for k1,v1 in pairs(t1) do
--     local v2 = t2[k1]
--     if v2 == nil or not deepcompare(v1,v2) then return false end
--     end
--     for k2,v2 in pairs(t2) do
--     local v1 = t1[k2]
--     if v1 == nil or not deepcompare(v1,v2) then return false end
--     end
--     return true
-- end

-- function mod.equals(a, b)
--     return deepcompare(a, b, true)
-- end
-- return mod

function TableConcat(t1,t2)
    for i=1,#t2 do
        t1[#t1+1] = t2[i]
    end
    return t1
end

-- https://stackoverflow.com/questions/5977654/how-do-i-use-the-bitwise-operator-xor-in-lua
local function BitXOR(a,b)--Bitwise xor
    local p,c=1,0
    while a>0 and b>0 do
        local ra,rb=a%2,b%2
        if ra~=rb then c=c+p end
        a,b,p=(a-ra)/2,(b-rb)/2,p*2
    end
    if a<b then a=b end
    while a>0 do
        local ra=a%2
        if ra>0 then c=c+p end
        a,p=(a-ra)/2,p*2
    end
    return c
end

local function BitOR(a,b)--Bitwise or
    local p,c=1,0
    while a+b>0 do
        local ra,rb=a%2,b%2
        if ra+rb>0 then c=c+p end
        a,b,p=(a-ra)/2,(b-rb)/2,p*2
    end
    return c
end

local function BitAND(a,b)--Bitwise and
    local p,c=1,0
    while a>0 and b>0 do
        local ra,rb=a%2,b%2
        if ra+rb>1 then c=c+p end
        a,b,p=(a-ra)/2,(b-rb)/2,p*2
    end
    return c
end

function lshift(x, by)
    return x * 2 ^ by
  end

  function rshift(x, by)
    return math.floor(x / 2 ^ by)
  end


--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
-- Lua Library inline imports
____symbolMetatable = {
    __tostring = function(self)
        return ("Symbol(" .. (self.description or "")) .. ")"
    end
}
function __TS__Symbol(description)
    return setmetatable({description = description}, ____symbolMetatable)
end
Symbol = {
    iterator = __TS__Symbol("Symbol.iterator"),
    hasInstance = __TS__Symbol("Symbol.hasInstance"),
    species = __TS__Symbol("Symbol.species"),
    toStringTag = __TS__Symbol("Symbol.toStringTag")
}

function __TS__ArrayIsArray(value)
    return (type(value) == "table") and ((value[1] ~= nil) or (next(value, nil) == nil))
end

function __TS__Class(self)
    local c = {prototype = {}}
    c.prototype.__index = c.prototype
    c.prototype.constructor = c
    return c
end

function __TS__ClassExtends(target, base)
    target.____super = base
    local staticMetatable = setmetatable({__index = base}, base)
    setmetatable(target, staticMetatable)
    local baseMetatable = getmetatable(base)
    if baseMetatable then
        if type(baseMetatable.__index) == "function" then
            staticMetatable.__index = baseMetatable.__index
        end
        if type(baseMetatable.__newindex) == "function" then
            staticMetatable.__newindex = baseMetatable.__newindex
        end
    end
    setmetatable(target.prototype, base.prototype)
    if type(base.prototype.__index) == "function" then
        target.prototype.__index = base.prototype.__index
    end
    if type(base.prototype.__newindex) == "function" then
        target.prototype.__newindex = base.prototype.__newindex
    end
    if type(base.prototype.__tostring) == "function" then
        target.prototype.__tostring = base.prototype.__tostring
    end
end

function __TS__New(target, ...)
    local instance = setmetatable({}, target.prototype)
    instance:____constructor(...)
    return instance
end

function __TS__GetErrorStack(self, constructor)
    local level = 1
    while true do
        local info = debug.getinfo(level, "f")
        level = level + 1
        if not info then
            level = 1
            break
        elseif info.func == constructor then
            break
        end
    end
    return debug.traceback(nil, level)
end
function __TS__WrapErrorToString(self, getDescription)
    return function(self)
        local description = getDescription(self)
        local caller = debug.getinfo(3, "f")
        if (_VERSION == "Lua 5.1") or (caller and (caller.func ~= error)) then
            return description
        else
            return (tostring(description) .. "\n") .. self.stack
        end
    end
end
function __TS__InitErrorClass(self, Type, name)
    Type.name = name
    return setmetatable(
        Type,
        {
            __call = function(____, _self, message) return __TS__New(Type, message) end
        }
    )
end
Error = __TS__InitErrorClass(
    _G,
    (function()
        local ____ = __TS__Class()
        ____.name = ""
        function ____.prototype.____constructor(self, message)
            if message == nil then
                message = ""
            end
            self.message = message
            self.name = "Error"
            self.stack = __TS__GetErrorStack(_G, self.constructor.new)
            local metatable = getmetatable(self)
            if not metatable.__errorToStringPatched then
                metatable.__errorToStringPatched = true
                metatable.__tostring = __TS__WrapErrorToString(_G, metatable.__tostring)
            end
        end
        function ____.prototype.__tostring(self)
            return (((self.message ~= "") and (function() return (self.name .. ": ") .. self.message end)) or (function() return self.name end))()
        end
        return ____
    end)(),
    "Error"
)
for ____, errorName in ipairs({"RangeError", "ReferenceError", "SyntaxError", "TypeError", "URIError"}) do
    _G[errorName] = __TS__InitErrorClass(
        _G,
        (function()
            local ____ = __TS__Class()
            ____.name = ____.name
            __TS__ClassExtends(____, Error)
            function ____.prototype.____constructor(self, ...)
                Error.prototype.____constructor(self, ...)
                self.name = errorName
            end
            return ____
        end)(),
        errorName
    )
end

function __TS__ObjectAssign(to, ...)
    local sources = {...}
    if to == nil then
        return to
    end
    for ____, source in ipairs(sources) do
        for key in pairs(source) do
            to[key] = source[key]
        end
    end
    return to
end

function __TS__CloneDescriptor(____bindingPattern0)
    local enumerable
    enumerable = ____bindingPattern0.enumerable
    local configurable
    configurable = ____bindingPattern0.configurable
    local get
    get = ____bindingPattern0.get
    local set
    set = ____bindingPattern0.set
    local writable
    writable = ____bindingPattern0.writable
    local value
    value = ____bindingPattern0.value
    local descriptor = {enumerable = enumerable == true, configurable = configurable == true}
    local hasGetterOrSetter = (get ~= nil) or (set ~= nil)
    local hasValueOrWritableAttribute = (writable ~= nil) or (value ~= nil)
    if hasGetterOrSetter and hasValueOrWritableAttribute then
        error("Invalid property descriptor. Cannot both specify accessors and a value or writable attribute.", 0)
    end
    if get or set then
        descriptor.get = get
        descriptor.set = set
    else
        descriptor.value = value
        descriptor.writable = writable == true
    end
    return descriptor
end

function ____descriptorIndex(self, key)
    local value = rawget(self, key)
    if value ~= nil then
        return value
    end
    local metatable = getmetatable(self)
    while metatable do
        local rawResult = rawget(metatable, key)
        if rawResult ~= nil then
            return rawResult
        end
        local descriptors = rawget(metatable, "_descriptors")
        if descriptors then
            local descriptor = descriptors[key]
            if descriptor then
                if descriptor.get then
                    return descriptor.get(self)
                end
                return descriptor.value
            end
        end
        metatable = getmetatable(metatable)
    end
end
function ____descriptorNewindex(self, key, value)
    local metatable = getmetatable(self)
    while metatable do
        local descriptors = rawget(metatable, "_descriptors")
        if descriptors then
            local descriptor = descriptors[key]
            if descriptor then
                if descriptor.set then
                    descriptor.set(self, value)
                else
                    if descriptor.writable == false then
                        error(
                            ((("Cannot assign to read only property '" .. key) .. "' of object '") .. tostring(self)) .. "'",
                            0
                        )
                    end
                    descriptor.value = value
                end
                return
            end
        end
        metatable = getmetatable(metatable)
    end
    rawset(self, key, value)
end
function __TS__SetDescriptor(target, key, desc, isPrototype)
    if isPrototype == nil then
        isPrototype = false
    end
    local metatable = ((isPrototype and (function() return target end)) or (function() return getmetatable(target) end))()
    if not metatable then
        metatable = {}
        setmetatable(target, metatable)
    end
    local value = rawget(target, key)
    if value ~= nil then
        rawset(target, key, nil)
    end
    if not rawget(metatable, "_descriptors") then
        metatable._descriptors = {}
    end
    local descriptor = __TS__CloneDescriptor(desc)
    metatable._descriptors[key] = descriptor
    metatable.__index = ____descriptorIndex
    metatable.__newindex = ____descriptorNewindex
end

function __TS__StringAccess(self, index)
    if (index >= 0) and (index < #self) then
        return string.sub(self, index + 1, index + 1)
    end
end

____radixChars = "0123456789abcdefghijklmnopqrstuvwxyz"
function __TS__NumberToString(self, radix)
    if ((((radix == nil) or (radix == 10)) or (self == math.huge)) or (self == -math.huge)) or (self ~= self) then
        return tostring(self)
    end
    radix = math.floor(radix)
    if (radix < 2) or (radix > 36) then
        error("toString() radix argument must be between 2 and 36", 0)
    end
    local integer, fraction = math.modf(
        math.abs(self)
    )
    local result = ""
    if radix == 8 then
        result = string.format("%o", integer)
    elseif radix == 16 then
        result = string.format("%x", integer)
    else
        repeat
            do
                result = __TS__StringAccess(____radixChars, integer % radix) .. result
                integer = math.floor(integer / radix)
            end
        until not (integer ~= 0)
    end
    if fraction ~= 0 then
        result = result .. "."
        local delta = 1e-16
        repeat
            do
                fraction = fraction * radix
                delta = delta * radix
                local digit = math.floor(fraction)
                result = result .. __TS__StringAccess(____radixChars, digit)
                fraction = fraction - digit
            end
        until not (fraction >= delta)
    end
    if self < 0 then
        result = "-" .. result
    end
    return result
end

function __TS__InstanceOf(obj, classTbl)
    if type(classTbl) ~= "table" then
        error("Right-hand side of 'instanceof' is not an object", 0)
    end
    if classTbl[Symbol.hasInstance] ~= nil then
        return not (not classTbl[Symbol.hasInstance](classTbl, obj))
    end
    if type(obj) == "table" then
        local luaClass = obj.constructor
        while luaClass ~= nil do
            if luaClass == classTbl then
                return true
            end
            luaClass = luaClass.____super
        end
    end
    return false
end

function __TS__IteratorGeneratorStep(self)
    local co = self.____coroutine
    local status, value = coroutine.resume(co)
    if not status then
        error(value, 0)
    end
    if coroutine.status(co) == "dead" then
        return
    end
    return true, value
end
function __TS__IteratorIteratorStep(self)
    local result = self:next()
    if result.done then
        return
    end
    return true, result.value
end
function __TS__IteratorStringStep(self, index)
    index = index + 1
    if index > #self then
        return
    end
    return index, string.sub(self, index, index)
end
function __TS__Iterator(iterable)
    if type(iterable) == "string" then
        return __TS__IteratorStringStep, iterable, 0
    elseif iterable.____coroutine ~= nil then
        return __TS__IteratorGeneratorStep, iterable
    elseif iterable[Symbol.iterator] then
        local iterator = iterable[Symbol.iterator](iterable)
        return __TS__IteratorIteratorStep, iterator
    else
        return ipairs(iterable)
    end
end

WeakMap = (function()
    local WeakMap = __TS__Class()
    WeakMap.name = "WeakMap"
    function WeakMap.prototype.____constructor(self, entries)
        self[Symbol.toStringTag] = "WeakMap"
        self.items = {}
        setmetatable(self.items, {__mode = "k"})
        if entries == nil then
            return
        end
        local iterable = entries
        if iterable[Symbol.iterator] then
            local iterator = iterable[Symbol.iterator](iterable)
            while true do
                local result = iterator:next()
                if result.done then
                    break
                end
                local value = result.value
                self.items[value[1]] = value[2]
            end
        else
            for ____, kvp in ipairs(entries) do
                self.items[kvp[1]] = kvp[2]
            end
        end
    end
    function WeakMap.prototype.delete(self, key)
        local contains = self:has(key)
        self.items[key] = nil
        return contains
    end
    function WeakMap.prototype.get(self, key)
        return self.items[key]
    end
    function WeakMap.prototype.has(self, key)
        return self.items[key] ~= nil
    end
    function WeakMap.prototype.set(self, key, value)
        self.items[key] = value
        return self
    end
    WeakMap[Symbol.species] = WeakMap
    return WeakMap
end)()

function __TS__StringCharCodeAt(self, index)
    if index ~= index then
        index = 0
    end
    if index < 0 then
        return 0 / 0
    end
    return string.byte(self, index + 1) or (0 / 0)
end

function __TS__ArrayReduce(arr, callbackFn, ...)
    local len = #arr
    local k = 0
    local accumulator = nil
    if select("#", ...) ~= 0 then
        accumulator = select(1, ...)
    elseif len > 0 then
        accumulator = arr[1]
        k = 1
    else
        error("Reduce of empty array with no initial value", 0)
    end
    for i = k, len - 1 do
        accumulator = callbackFn(_G, accumulator, arr[i + 1], i, arr)
    end
    return accumulator
end

function __TS__TypeOf(value)
    local luaType = type(value)
    if luaType == "table" then
        return "object"
    elseif luaType == "nil" then
        return "undefined"
    else
        return luaType
    end
end

function __TS__ObjectValues(obj)
    local result = {}
    for key in pairs(obj) do
        result[#result + 1] = obj[key]
    end
    return result
end

function __TS__ArrayMap(arr, callbackfn)
    local newArray = {}
    do
        local i = 0
        while i < #arr do
            newArray[i + 1] = callbackfn(_G, arr[i + 1], i, arr)
            i = i + 1
        end
    end
    return newArray
end

function __TS__ObjectKeys(obj)
    local result = {}
    for key in pairs(obj) do
        result[#result + 1] = key
    end
    return result
end

function __TS__ArraySort(arr, compareFn)
    if compareFn ~= nil then
        table.sort(
            arr,
            function(a, b) return compareFn(_G, a, b) < 0 end
        )
    else
        table.sort(arr)
    end
    return arr
end

function __TS__StringReplace(source, searchValue, replaceValue)
    searchValue = string.gsub(searchValue, "[%%%(%)%.%+%-%*%?%[%^%$]", "%%%1")
    if type(replaceValue) == "string" then
        replaceValue = string.gsub(replaceValue, "%%", "%%%%")
        local result = string.gsub(source, searchValue, replaceValue, 1)
        return result
    else
        local result = string.gsub(
            source,
            searchValue,
            function(match) return replaceValue(_G, match) end,
            1
        )
        return result
    end
end

function __TS__ArraySplice(list, ...)
    local len = #list
    local actualArgumentCount = select("#", ...)
    local start = select(1, ...)
    local deleteCount = select(2, ...)
    local actualStart
    if start < 0 then
        actualStart = math.max(len + start, 0)
    else
        actualStart = math.min(start, len)
    end
    local itemCount = math.max(actualArgumentCount - 2, 0)
    local actualDeleteCount
    if actualArgumentCount == 0 then
        actualDeleteCount = 0
    elseif actualArgumentCount == 1 then
        actualDeleteCount = len - actualStart
    else
        actualDeleteCount = math.min(
            math.max(deleteCount or 0, 0),
            len - actualStart
        )
    end
    local out = {}
    do
        local k = 0
        while k < actualDeleteCount do
            local from = actualStart + k
            if list[from + 1] then
                out[k + 1] = list[from + 1]
            end
            k = k + 1
        end
    end
    if itemCount < actualDeleteCount then
        do
            local k = actualStart
            while k < (len - actualDeleteCount) do
                local from = k + actualDeleteCount
                local to = k + itemCount
                if list[from + 1] then
                    list[to + 1] = list[from + 1]
                else
                    list[to + 1] = nil
                end
                k = k + 1
            end
        end
        do
            local k = len
            while k > ((len - actualDeleteCount) + itemCount) do
                list[k] = nil
                k = k - 1
            end
        end
    elseif itemCount > actualDeleteCount then
        do
            local k = len - actualDeleteCount
            while k > actualStart do
                local from = (k + actualDeleteCount) - 1
                local to = (k + itemCount) - 1
                if list[from + 1] then
                    list[to + 1] = list[from + 1]
                else
                    list[to + 1] = nil
                end
                k = k - 1
            end
        end
    end
    local j = actualStart
    for i = 3, actualArgumentCount do
        list[j + 1] = select(i, ...)
        j = j + 1
    end
    do
        local k = #list - 1
        while k >= ((len - actualDeleteCount) + itemCount) do
            list[k + 1] = nil
            k = k - 1
        end
    end
    return out
end

function __TS__ArrayConcat(arr1, ...)
    local args = {...}
    local out = {}
    for ____, val in ipairs(arr1) do
        out[#out + 1] = val
    end
    for ____, arg in ipairs(args) do
        if __TS__ArrayIsArray(arg) then
            local argAsArray = arg
            for ____, val in ipairs(argAsArray) do
                out[#out + 1] = val
            end
        else
            out[#out + 1] = arg
        end
    end
    return out
end

local ____exports = {}
local isComparable, isEquatable, isHashable, equalObjects, compareObjects
function ____exports.isArrayLike(self, x)
    return __TS__ArrayIsArray(x) or ArrayBuffer:isView(x)
end
function isComparable(self, x)
    return type(x.CompareTo) == "function"
end
function isEquatable(self, x)
    return type(x.Equals) == "function"
end
function isHashable(self, x)
    return type(x.GetHashCode) == "function"
end
function ____exports.dateOffset(self, date)
    local date1 = date
    return ((type(date1.offset) == "number") and date1.offset) or (((date.kind == 1) and 0) or (date:getTimezoneOffset() * -60000))
end
function ____exports.stringHash(self, s)
    local i = 0
    local h = 5381
    local len = #s
    while i < len do
        h = BitXOR((h * 33), __TS__StringCharCodeAt(
            s,
            (function()
                local ____tmp = i
                i = ____tmp + 1
                return ____tmp
            end)()
        ))
    end
    return h
end
function ____exports.numberHash(self, x)
    return BitOR((x * 2654435761), 0)
end
function ____exports.combineHashCodes(self, hashes)
    if #hashes == 0 then
        return 0
    end
    return __TS__ArrayReduce(
        hashes,
        function(____, h1, h2)
            return BitXOR((lshift(h1, 5) + h1), h2)
        end
    )
end
function ____exports.dateHash(self, x)
    return x:getTime()
end
function ____exports.arrayHash(self, x)
    local len = x.length
    local hashes = __TS__New(Array, len)
    do
        local i = 0
        while i < len do
            hashes[i + 1] = ____exports.structuralHash(nil, x[i])
            i = i + 1
        end
    end
    return ____exports.combineHashCodes(nil, hashes)
end
function ____exports.structuralHash(self, x)
    if x == nil then
        return 0
    end
    local ____switch68 = __TS__TypeOf(x)
    if ____switch68 == "boolean" then
        goto ____switch68_case_0
    elseif ____switch68 == "number" then
        goto ____switch68_case_1
    elseif ____switch68 == "string" then
        goto ____switch68_case_2
    end
    goto ____switch68_case_default
    ::____switch68_case_0::
    do
        return (x and 1) or 0
    end
    ::____switch68_case_1::
    do
        return ____exports.numberHash(nil, x)
    end
    ::____switch68_case_2::
    do
        return ____exports.stringHash(nil, x)
    end
    ::____switch68_case_default::
    do
        do
            if isHashable(nil, x) then
                return x:GetHashCode()
            elseif ____exports.isArrayLike(nil, x) then
                return ____exports.arrayHash(nil, x)
            elseif __TS__InstanceOf(x, Date) then
                return ____exports.dateHash(nil, x)
            elseif Object:getPrototypeOf(x).constructor == Object then
                local hashes = __TS__ArrayMap(
                    __TS__ObjectValues(x),
                    function(____, v) return ____exports.structuralHash(nil, v) end
                )
                return ____exports.combineHashCodes(nil, hashes)
            else
                return ____exports.numberHash(
                    nil,
                    ____exports.ObjectRef:id(x)
                )
            end
        end
    end
    ::____switch68_end::
end
function ____exports.equalArraysWith(self, x, y, eq)
    if x == nil then
        return y == nil
    end
    if y == nil then
        return false
    end
    if x.length ~= y.length then
        return false
    end
    do
        local i = 0
        while i < x.length do
            if not eq(nil, x[i], y[i]) then
                return false
            end
            i = i + 1
        end
    end
    return true
end
function ____exports.equalArrays(self, x, y)
    return ____exports.equalArraysWith(nil, x, y, ____exports.equals)
end
function equalObjects(self, x, y)
    local xKeys = __TS__ObjectKeys(x)
    local yKeys = __TS__ObjectKeys(y)
    if #xKeys ~= #yKeys then
        return false
    end
    __TS__ArraySort(xKeys)
    __TS__ArraySort(yKeys)
    do
        local i = 0
        while i < #xKeys do
            if (xKeys[i + 1] ~= yKeys[i + 1]) or (not ____exports.equals(nil, x[xKeys[i + 1]], y[yKeys[i + 1]])) then
                return false
            end
            i = i + 1
        end
    end
    return true
end
function ____exports.equals(self, x, y)
    if x == y then
        return true
    elseif x == nil then
        return y == nil
    elseif y == nil then
        return false
    elseif type(x) ~= "table" then
        return false
    elseif isEquatable(nil, x) then
        return x:Equals(y)
    elseif ____exports.isArrayLike(nil, x) then
        return ____exports.isArrayLike(nil, y) and ____exports.equalArrays(nil, x, y)
    elseif __TS__InstanceOf(x, Date) then
        return __TS__InstanceOf(y, Date) and (____exports.compareDates(nil, x, y) == 0)
    else
        return (Object:getPrototypeOf(x).constructor == Object) and equalObjects(nil, x, y)
    end
end
function ____exports.compareDates(self, x, y)
    local xtime
    local ytime
    if (x.offset ~= nil) and (y.offset ~= nil) then
        xtime = x:getTime()
        ytime = y:getTime()
    else
        xtime = x:getTime() + ____exports.dateOffset(nil, x)
        ytime = y:getTime() + ____exports.dateOffset(nil, y)
    end
    return ((xtime == ytime) and 0) or (((xtime < ytime) and -1) or 1)
end
function ____exports.compareArraysWith(self, x, y, comp)
    if x == nil then
        return ((y == nil) and 0) or 1
    end
    if y == nil then
        return -1
    end
    if x.length ~= y.length then
        return ((x.length < y.length) and -1) or 1
    end
    do
        local i = 0
        local j = 0
        while i < x.length do
            j = comp(nil, x[i], y[i])
            if j ~= 0 then
                return j
            end
            i = i + 1
        end
    end
    return 0
end
function ____exports.compareArrays(self, x, y)
    return ____exports.compareArraysWith(nil, x, y, ____exports.compare)
end
function compareObjects(self, x, y)
    local xKeys = __TS__ObjectKeys(x)
    local yKeys = __TS__ObjectKeys(y)
    if #xKeys ~= #yKeys then
        return ((#xKeys < #yKeys) and -1) or 1
    end
    __TS__ArraySort(xKeys)
    __TS__ArraySort(yKeys)
    do
        local i = 0
        local j = 0
        while i < #xKeys do
            local key = xKeys[i + 1]
            if key ~= yKeys[i + 1] then
                return ((key < yKeys[i + 1]) and -1) or 1
            else
                j = ____exports.compare(nil, x[key], y[key])
                if j ~= 0 then
                    return j
                end
            end
            i = i + 1
        end
    end
    return 0
end
function ____exports.compare(self, x, y)
    if x == y then
        return 0
    elseif x == nil then
        return ((y == nil) and 0) or -1
    elseif y == nil then
        return 1
    elseif type(x) ~= "table" then
        return ((x < y) and -1) or 1
    elseif isComparable(nil, x) then
        return x:CompareTo(y)
    elseif ____exports.isArrayLike(nil, x) then
        return (____exports.isArrayLike(nil, y) and ____exports.compareArrays(nil, x, y)) or -1
    elseif __TS__InstanceOf(x, Date) then
        return (__TS__InstanceOf(y, Date) and ____exports.compareDates(nil, x, y)) or -1
    else
        return ((Object:getPrototypeOf(x).constructor == Object) and compareObjects(nil, x, y)) or -1
    end
end
function ____exports.isIterable(self, x)
    return ((x ~= nil) and (type(x) == "table")) and (x[Symbol.iterator] ~= nil)
end
local function isComparer(self, x)
    return type(x.Compare) == "function"
end
function ____exports.isDisposable(self, x)
    return (x ~= nil) and (type(x.Dispose) == "function")
end
function ____exports.sameConstructor(self, x, y)
    return Object:getPrototypeOf(x).constructor == Object:getPrototypeOf(y).constructor
end
____exports.Enumerator = __TS__Class()
local Enumerator = ____exports.Enumerator
Enumerator.name = "Enumerator"
function Enumerator.prototype.____constructor(self, iter)
    self.iter = iter
end
Enumerator.prototype["System.Collections.Generic.IEnumerator`1.get_Current"] = function(self)
    return self.current
end
Enumerator.prototype["System.Collections.IEnumerator.get_Current"] = function(self)
    return self.current
end
Enumerator.prototype["System.Collections.IEnumerator.MoveNext"] = function(self)
    local cur = self.iter:next()
    self.current = cur.value
    return not cur.done
end
Enumerator.prototype["System.Collections.IEnumerator.Reset"] = function(self)
    error(
        __TS__New(Error, "JS iterators cannot be reset"),
        0
    )
end
function Enumerator.prototype.Dispose(self)
    return
end
function ____exports.getEnumerator(self, o)
    return ((type(o.GetEnumerator) == "function") and o:GetEnumerator()) or __TS__New(
        ____exports.Enumerator,
        o[Symbol.iterator](o)
    )
end
function ____exports.toIterator(self, en)
    return {
        [Symbol.iterator] = function(self)
            return self
        end,
        next = function(self)
            local hasNext = en["System.Collections.IEnumerator.MoveNext"](en)
            local current = ((hasNext and (function() return en["System.Collections.IEnumerator.get_Current"](en) end)) or (function() return nil end))()
            return {done = not hasNext, value = current}
        end
    }
end
____exports.Comparer = __TS__Class()
local Comparer = ____exports.Comparer
Comparer.name = "Comparer"
function Comparer.prototype.____constructor(self, f)
    self.Compare = f or ____exports.compare
end
function ____exports.comparerFromEqualityComparer(self, comparer)
    if isComparer(nil, comparer) then
        return __TS__New(____exports.Comparer, comparer.Compare)
    else
        return __TS__New(
            ____exports.Comparer,
            function(____, x, y)
                local xhash = comparer:GetHashCode(x)
                local yhash = comparer:GetHashCode(y)
                if xhash == yhash then
                    return (comparer:Equals(x, y) and 0) or -1
                else
                    return ((xhash < yhash) and -1) or 1
                end
            end
        )
    end
end
function ____exports.assertEqual(self, actual, expected, msg)
    if not ____exports.equals(nil, actual, expected) then
        error(
            __TS__ObjectAssign(
                __TS__New(
                    Error,
                    msg or ((("Expected: " .. tostring(expected)) .. " - Actual: ") .. tostring(actual))
                ),
                {actual = actual, expected = expected}
            ),
            0
        )
    end
end
function ____exports.assertNotEqual(self, actual, expected, msg)
    if ____exports.equals(nil, actual, expected) then
        error(
            __TS__ObjectAssign(
                __TS__New(
                    Error,
                    msg or ((("Expected: " .. tostring(expected)) .. " - Actual: ") .. tostring(actual))
                ),
                {actual = actual, expected = expected}
            ),
            0
        )
    end
end
____exports.Lazy = __TS__Class()
local Lazy = ____exports.Lazy
Lazy.name = "Lazy"
function Lazy.prototype.____constructor(self, factory)
    self.factory = factory
    self.isValueCreated = false
end
__TS__SetDescriptor(
    Lazy.prototype,
    "Value",
    {
        get = function(self)
            if not self.isValueCreated then
                self.createdValue = self:factory()
                self.isValueCreated = true
            end
            return self.createdValue
        end
    },
    true
)
__TS__SetDescriptor(
    Lazy.prototype,
    "IsValueCreated",
    {
        get = function(self)
            return self.isValueCreated
        end
    },
    true
)
function ____exports.lazyFromValue(self, v)
    return __TS__New(
        ____exports.Lazy,
        function() return v end
    )
end
function ____exports.padWithZeros(self, i, length)
    local str = __TS__NumberToString(i, 10)
    while #str < length do
        str = "0" .. str
    end
    return str
end
function ____exports.padLeftAndRightWithZeros(self, i, lengthLeft, lengthRight)
    local str = __TS__NumberToString(i, 10)
    while #str < lengthLeft do
        str = "0" .. str
    end
    while #str < lengthRight do
        str = str .. "0"
    end
    return str
end
function ____exports.int16ToString(self, i, radix)
    i = ((((i < 0) and (radix ~= nil)) and (radix ~= 10)) and ((65535 + i) + 1)) or i
    return __TS__NumberToString(i, radix)
end
function ____exports.int32ToString(self, i, radix)
    i = ((((i < 0) and (radix ~= nil)) and (radix ~= 10)) and ((4294967295 + i) + 1)) or i
    return __TS__NumberToString(i, radix)
end
____exports.ObjectRef = __TS__Class()
local ObjectRef = ____exports.ObjectRef
ObjectRef.name = "ObjectRef"
function ObjectRef.prototype.____constructor(self)
end
function ObjectRef.id(self, o)
    if not ____exports.ObjectRef.idMap:has(o) then
        ____exports.ObjectRef.idMap:set(
            o,
            (function()
                local ____tmp = ____exports.ObjectRef.count + 1
                ____exports.ObjectRef.count = ____tmp
                return ____tmp
            end)()
        )
    end
    return ____exports.ObjectRef.idMap:get(o)
end
ObjectRef.idMap = __TS__New(WeakMap)
ObjectRef.count = 0
function ____exports.physicalHash(self, x)
    if x == nil then
        return 0
    end
    local ____switch58 = __TS__TypeOf(x)
    if ____switch58 == "boolean" then
        goto ____switch58_case_0
    elseif ____switch58 == "number" then
        goto ____switch58_case_1
    elseif ____switch58 == "string" then
        goto ____switch58_case_2
    end
    goto ____switch58_case_default
    ::____switch58_case_0::
    do
        return (x and 1) or 0
    end
    ::____switch58_case_1::
    do
        return ____exports.numberHash(nil, x)
    end
    ::____switch58_case_2::
    do
        return ____exports.stringHash(nil, x)
    end
    ::____switch58_case_default::
    do
        return ____exports.numberHash(
            nil,
            ____exports.ObjectRef:id(x)
        )
    end
    ::____switch58_end::
end
function ____exports.identityHash(self, x)
    if x == nil then
        return 0
    elseif isHashable(nil, x) then
        return x:GetHashCode()
    else
        return ____exports.physicalHash(nil, x)
    end
end
function ____exports.fastStructuralHash(self, x)
    return ____exports.stringHash(
        nil,
        String(nil, x)
    )
end
function ____exports.safeHash(self, x)
    return ((x == nil) and 0) or ((isHashable(nil, x) and x:GetHashCode()) or ____exports.numberHash(
        nil,
        ____exports.ObjectRef:id(x)
    ))
end
function ____exports.comparePrimitives(self, x, y)
    return ((x == y) and 0) or (((x < y) and -1) or 1)
end
function ____exports.min(self, comparer, x, y)
    return ((comparer(nil, x, y) < 0) and x) or y
end
function ____exports.max(self, comparer, x, y)
    return ((comparer(nil, x, y) > 0) and x) or y
end
function ____exports.clamp(self, comparer, value, min, max)
    return ((comparer(nil, value, min) < 0) and min) or (((comparer(nil, value, max) > 0) and max) or value)
end
function ____exports.createAtom(self, value)
    local atom = value
    return function(____, value, isSetter)
        if not isSetter then
            return atom
        else
            atom = value
            return nil
        end
    end
end
function ____exports.createObj(self, fields)
    local obj = {}
    for ____, kv in __TS__Iterator(fields) do
        obj[kv[1]] = kv[2]
    end
    return obj
end
function ____exports.jsOptions(self, mutator)
    local opts = {}
    mutator(nil, opts)
    return opts
end
function ____exports.round(self, value, digits)
    if digits == nil then
        digits = 0
    end
    local m = math.pow(10, digits)
    local n = ((digits and (value * m)) or value):toFixed(8)
    local i = math.floor(n)
    local f = n - i
    local e = 1e-8
    local r = (((f > (0.5 - e)) and (f < (0.5 + e))) and ((((i % 2) == 0) and i) or (i + 1))) or math.floor(n + 0.5)
    return (digits and (r / m)) or r
end
function ____exports.sign(self, x)
    return ((x > 0) and 1) or (((x < 0) and -1) or 0)
end
function ____exports.randomNext(self, min, max)
    return math.floor(
        math.random() * (max - min)
    ) + min
end
function ____exports.randomBytes(self, buffer)
    if buffer == nil then
        error(
            __TS__New(Error, "Buffer cannot be null"),
            0
        )
    end
    do
        local i = 0
        while i < buffer.length do
            local r = math.floor(
                math.random() * 281474976710656
            )
            local rhi = math.floor(r / 16777216)
            do
                local j = 0
                while (j < 6) and ((i + j) < buffer.length) do
                    if j == 3 then
                        r = rhi
                    end
                    buffer[i + j] = BitAND(r, 255)
                    r = rshift(r, 8)
                    j = j + 1
                end
            end
            i = i + 6
        end
    end
end
function ____exports.unescapeDataString(self, s)
    return decodeURIComponent(
        nil,
        __TS__StringReplace(s, nil, "%20")
    )
end
function ____exports.escapeDataString(self, s)
    return __TS__StringReplace(
        __TS__StringReplace(
            __TS__StringReplace(
                __TS__StringReplace(
                    __TS__StringReplace(
                        encodeURIComponent(nil, s),
                        nil,
                        "%21"
                    ),
                    nil,
                    "%27"
                ),
                nil,
                "%28"
            ),
            nil,
            "%29"
        ),
        nil,
        "%2A"
    )
end
function ____exports.escapeUriString(self, s)
    return encodeURI(nil, s)
end
function ____exports.count(self, col)
    if ____exports.isArrayLike(nil, col) then
        return col.length
    else
        local count = 0
        for ____, _ in __TS__Iterator(col) do
            count = count + 1
        end
        return count
    end
end
function ____exports.clear(self, col)
    if ____exports.isArrayLike(nil, col) then
        __TS__ArraySplice(col, 0)
    else
        col:clear()
    end
end
local CURRIED_KEY = "__CURRIED__"
function ____exports.uncurry(self, arity, f)
    if (f == nil) or (f.length > 1) then
        return f
    end
    local uncurriedFn
    local ____switch154 = arity
    if ____switch154 == 2 then
        goto ____switch154_case_0
    elseif ____switch154 == 3 then
        goto ____switch154_case_1
    elseif ____switch154 == 4 then
        goto ____switch154_case_2
    elseif ____switch154 == 5 then
        goto ____switch154_case_3
    elseif ____switch154 == 6 then
        goto ____switch154_case_4
    elseif ____switch154 == 7 then
        goto ____switch154_case_5
    elseif ____switch154 == 8 then
        goto ____switch154_case_6
    end
    goto ____switch154_case_default
    ::____switch154_case_0::
    do
        uncurriedFn = function(____, a1, a2) return f(nil, a1)(nil, a2) end
        goto ____switch154_end
    end
    ::____switch154_case_1::
    do
        uncurriedFn = function(____, a1, a2, a3) return f(nil, a1)(nil, a2)(nil, a3) end
        goto ____switch154_end
    end
    ::____switch154_case_2::
    do
        uncurriedFn = function(____, a1, a2, a3, a4) return f(nil, a1)(nil, a2)(nil, a3)(nil, a4) end
        goto ____switch154_end
    end
    ::____switch154_case_3::
    do
        uncurriedFn = function(____, a1, a2, a3, a4, a5) return f(nil, a1)(nil, a2)(nil, a3)(nil, a4)(nil, a5) end
        goto ____switch154_end
    end
    ::____switch154_case_4::
    do
        uncurriedFn = function(____, a1, a2, a3, a4, a5, a6) return f(nil, a1)(nil, a2)(nil, a3)(nil, a4)(nil, a5)(nil, a6) end
        goto ____switch154_end
    end
    ::____switch154_case_5::
    do
        uncurriedFn = function(____, a1, a2, a3, a4, a5, a6, a7) return f(nil, a1)(nil, a2)(nil, a3)(nil, a4)(nil, a5)(nil, a6)(nil, a7) end
        goto ____switch154_end
    end
    ::____switch154_case_6::
    do
        uncurriedFn = function(____, a1, a2, a3, a4, a5, a6, a7, a8) return f(nil, a1)(nil, a2)(nil, a3)(nil, a4)(nil, a5)(nil, a6)(nil, a7)(nil, a8) end
        goto ____switch154_end
    end
    ::____switch154_case_default::
    do
        error(
            __TS__New(
                Error,
                "Uncurrying to more than 8-arity is not supported: " .. tostring(arity)
            ),
            0
        )
    end
    ::____switch154_end::
    uncurriedFn[CURRIED_KEY] = f
    return uncurriedFn
end
function ____exports.curry(self, arity, f)
    if (f == nil) or (f.length == 1) then
        return f
    end
    if f[CURRIED_KEY] ~= nil then
        return f[CURRIED_KEY]
    end
    local ____switch165 = arity
    if ____switch165 == 2 then
        goto ____switch165_case_0
    elseif ____switch165 == 3 then
        goto ____switch165_case_1
    elseif ____switch165 == 4 then
        goto ____switch165_case_2
    elseif ____switch165 == 5 then
        goto ____switch165_case_3
    elseif ____switch165 == 6 then
        goto ____switch165_case_4
    elseif ____switch165 == 7 then
        goto ____switch165_case_5
    elseif ____switch165 == 8 then
        goto ____switch165_case_6
    end
    goto ____switch165_case_default
    ::____switch165_case_0::
    do
        return function(____, a1) return function(____, a2) return f(nil, a1, a2) end end
    end
    ::____switch165_case_1::
    do
        return function(____, a1) return function(____, a2) return function(____, a3) return f(nil, a1, a2, a3) end end end
    end
    ::____switch165_case_2::
    do
        return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return f(nil, a1, a2, a3, a4) end end end end
    end
    ::____switch165_case_3::
    do
        return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return function(____, a5) return f(nil, a1, a2, a3, a4, a5) end end end end end
    end
    ::____switch165_case_4::
    do
        return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return function(____, a5) return function(____, a6) return f(nil, a1, a2, a3, a4, a5, a6) end end end end end end
    end
    ::____switch165_case_5::
    do
        return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return function(____, a5) return function(____, a6) return function(____, a7) return f(nil, a1, a2, a3, a4, a5, a6, a7) end end end end end end end
    end
    ::____switch165_case_6::
    do
        return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return function(____, a5) return function(____, a6) return function(____, a7) return function(____, a8) return f(nil, a1, a2, a3, a4, a5, a6, a7, a8) end end end end end end end end
    end
    ::____switch165_case_default::
    do
        error(
            __TS__New(
                Error,
                "Currying to more than 8-arity is not supported: " .. tostring(arity)
            ),
            0
        )
    end
    ::____switch165_end::
end
function ____exports.checkArity(self, arity, f)
    return ((f.length > arity) and (function(____, ...)
        local args1 = {...}
        return function(____, ...)
            local args2 = {...}
            return f:apply(
                nil,
                __TS__ArrayConcat(args1, args2)
            )
        end
    end)) or f
end
function ____exports.partialApply(self, arity, f, args)
    if arity == 1 then
        return function (a) return f(table.unpack(__TS__ArrayConcat({ table.unpack(args) }, a))) end
    elseif arity == 2 then
        return function (a, b) return f(table.unpack(__TS__ArrayConcat({ table.unpack(args) }, a, b))) end
    end
    -- if f == nil then
    --     return nil
    -- elseif type(f) == 'table' and f[CURRIED_KEY] ~= nil then
    --     f = f[CURRIED_KEY]
    --     do
    --         local i = 0
    --         while i < #args do
    --             f = f(nil, args[i + 1])
    --             i = i + 1
    --         end
    --     end
    --     return f
    -- else
    --     local ____switch209 = arity
    --     if ____switch209 == 1 then
    --         goto ____switch209_case_0
    --     elseif ____switch209 == 2 then
    --         goto ____switch209_case_1
    --     elseif ____switch209 == 3 then
    --         goto ____switch209_case_2
    --     elseif ____switch209 == 4 then
    --         goto ____switch209_case_3
    --     elseif ____switch209 == 5 then
    --         goto ____switch209_case_4
    --     elseif ____switch209 == 6 then
    --         goto ____switch209_case_5
    --     elseif ____switch209 == 7 then
    --         goto ____switch209_case_6
    --     elseif ____switch209 == 8 then
    --         goto ____switch209_case_7
    --     end
    --     goto ____switch209_case_default
    --     ::____switch209_case_0::
    --     do
    --         return function(____, a1) return f:apply(
    --             nil,
    --             __TS__ArrayConcat(args, {a1})
    --         ) end
    --     end
    --     ::____switch209_case_1::
    --     do
    --         return function(____, a1) return function(____, a2) return f:apply(
    --             nil,
    --             __TS__ArrayConcat(args, {a1, a2})
    --         ) end end
    --     end
    --     ::____switch209_case_2::
    --     do
    --         return function(____, a1) return function(____, a2) return function(____, a3) return f:apply(
    --             nil,
    --             __TS__ArrayConcat(args, {a1, a2, a3})
    --         ) end end end
    --     end
    --     ::____switch209_case_3::
    --     do
    --         return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return f:apply(
    --             nil,
    --             __TS__ArrayConcat(args, {a1, a2, a3, a4})
    --         ) end end end end
    --     end
    --     ::____switch209_case_4::
    --     do
    --         return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return function(____, a5) return f:apply(
    --             nil,
    --             __TS__ArrayConcat(args, {a1, a2, a3, a4, a5})
    --         ) end end end end end
    --     end
    --     ::____switch209_case_5::
    --     do
    --         return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return function(____, a5) return function(____, a6) return f:apply(
    --             nil,
    --             __TS__ArrayConcat(args, {a1, a2, a3, a4, a5, a6})
    --         ) end end end end end end
    --     end
    --     ::____switch209_case_6::
    --     do
    --         return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return function(____, a5) return function(____, a6) return function(____, a7) return f:apply(
    --             nil,
    --             __TS__ArrayConcat(args, {a1, a2, a3, a4, a5, a6, a7})
    --         ) end end end end end end end
    --     end
    --     ::____switch209_case_7::
    --     do
    --         return function(____, a1) return function(____, a2) return function(____, a3) return function(____, a4) return function(____, a5) return function(____, a6) return function(____, a7) return function(____, a8) return f:apply(
    --             nil,
    --             __TS__ArrayConcat(args, {a1, a2, a3, a4, a5, a6, a7, a8})
    --         ) end end end end end end end end
    --     end
    --     ::____switch209_case_default::
    --     do
    --         error(
    --             __TS__New(
    --                 Error,
    --                 "Partially applying to more than 8-arity is not supported: " .. tostring(arity)
    --             ),
    --             0
    --         )
    --     end
    --     ::____switch209_end::
    -- end
end
function ____exports.mapCurriedArgs(self, fn, mappings)
    local function mapArg(self, fn, arg, mappings, idx)
        local mapping = mappings[idx + 1]
        if mapping ~= 0 then
            local expectedArity = mapping[1]
            local actualArity = mapping[2]
            if expectedArity > 1 then
                arg = ____exports.curry(nil, expectedArity, arg)
            end
            if actualArity > 1 then
                arg = ____exports.uncurry(nil, actualArity, arg)
            end
        end
        local res = fn(nil, arg)
        if (idx + 1) == #mappings then
            return res
        else
            return function(____, arg) return mapArg(nil, res, arg, mappings, idx + 1) end
        end
    end
    return function(____, arg) return mapArg(nil, fn, arg, mappings, 0) end
end
return ____exports