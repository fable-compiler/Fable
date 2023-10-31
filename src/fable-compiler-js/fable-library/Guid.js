import { trim } from "./String.js";
// RFC 4122 compliant. From https://stackoverflow.com/a/13653180/3922220
// const guidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;
// Relax GUID parsing, see #1637
const guidRegex = /^[\(\{]{0,2}[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}[\)\}]{0,2}$/;
const guidRegexNoHyphen = /^([0-9a-f]{8})([0-9a-f]{4})([0-9a-f]{4})([0-9a-f]{4})([0-9a-f]{12})$/;
const guidRegexHex = /^\{0x[0-9a-f]{8},(0x[0-9a-f]{4},){2}\{(0x[0-9a-f]{2},){7}0x[0-9a-f]{2}\}\}$/;
const guidHexCaptures = /^([0-9a-f]{8})-(([0-9a-f]{4})-)(([0-9a-f]{4})-)([0-9a-f]{2})([0-9a-f]{2})-([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/;
export function toString(str, format, _provider) {
    if (format && format?.length > 0) {
        switch (format) {
            case "N":
                return str.replace(/-/g, '');
            case "D":
                return str;
            case "B":
                return "{" + str + "}";
            case "P":
                return "(" + str + ")";
            case "X":
                return str.replace(guidHexCaptures, "{0x$1,0x$3,0x$5,{0x$6,0x$7,0x$8,0x$9,0x$10,0x$11,0x$12,0x$13}}");
            default:
                throw new Error("Unrecognized Guid print format");
        }
    }
    else {
        return str;
    }
}
/** Validates UUID as specified in RFC4122 (versions 1-5). */
export function parse(str) {
    function hyphenateGuid(str) {
        return str.replace(guidRegexNoHyphen, "$1-$2-$3-$4-$5");
    }
    const wsTrimAndLowered = str.trim().toLowerCase();
    if (guidRegex.test(wsTrimAndLowered)) {
        return trim(wsTrimAndLowered, "{", "}", "(", ")");
    }
    else if (guidRegexNoHyphen.test(wsTrimAndLowered)) {
        return hyphenateGuid(wsTrimAndLowered);
    }
    else if (guidRegexHex.test(wsTrimAndLowered)) {
        return hyphenateGuid(wsTrimAndLowered.replace(/[\{\},]|0x/g, ''));
    }
    else {
        throw new Error("Guid should contain 32 digits with 4 dashes: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx");
    }
}
export function tryParse(str, defValue) {
    try {
        defValue.contents = parse(str);
        return true;
    }
    catch {
        return false;
    }
}
// From https://gist.github.com/LeverOne/1308368
export function newGuid() {
    let b = "";
    for (let a = 0; a++ < 36;) {
        b += a * 51 & 52
            ? (a ^ 15 ? 8 ^ Math.random() * (a ^ 20 ? 16 : 4) : 4).toString(16)
            : "-";
    }
    return b;
}
// Maps for number <-> hex string conversion
let _convertMapsInitialized = false;
let _byteToHex;
let _hexToByte;
function initConvertMaps() {
    _byteToHex = new Array(256);
    _hexToByte = {};
    for (let i = 0; i < 256; i++) {
        _byteToHex[i] = (i + 0x100).toString(16).substr(1);
        _hexToByte[_byteToHex[i]] = i;
    }
    _convertMapsInitialized = true;
}
/** Parse a UUID into it's component bytes */
// Adapted from https://github.com/zefferus/uuid-parse
export function guidToArray(s) {
    if (!_convertMapsInitialized) {
        initConvertMaps();
    }
    let i = 0;
    const buf = new Uint8Array(16);
    s.toLowerCase().replace(/[0-9a-f]{2}/g, ((oct) => {
        switch (i) {
            // .NET saves first three byte groups with different endianness
            // See https://stackoverflow.com/a/16722909/3922220
            case 0:
            case 1:
            case 2:
            case 3:
                buf[3 - i++] = _hexToByte[oct];
                break;
            case 4:
            case 5:
                buf[9 - i++] = _hexToByte[oct];
                break;
            case 6:
            case 7:
                buf[13 - i++] = _hexToByte[oct];
                break;
            case 8:
            case 9:
            case 10:
            case 11:
            case 12:
            case 13:
            case 14:
            case 15:
                buf[i++] = _hexToByte[oct];
                break;
        }
    }));
    // Zero out remaining bytes if string was short
    while (i < 16) {
        buf[i++] = 0;
    }
    return buf;
}
/** Convert UUID byte array into a string */
export function arrayToGuid(buf) {
    if (buf.length !== 16) {
        throw new Error("Byte array for GUID must be exactly 16 bytes long");
    }
    if (!_convertMapsInitialized) {
        initConvertMaps();
    }
    const guid = _byteToHex[buf[3]] + _byteToHex[buf[2]] +
        _byteToHex[buf[1]] + _byteToHex[buf[0]] + "-" +
        _byteToHex[buf[5]] + _byteToHex[buf[4]] + "-" +
        _byteToHex[buf[7]] + _byteToHex[buf[6]] + "-" +
        _byteToHex[buf[8]] + _byteToHex[buf[9]] + "-" +
        _byteToHex[buf[10]] + _byteToHex[buf[11]] +
        _byteToHex[buf[12]] + _byteToHex[buf[13]] +
        _byteToHex[buf[14]] + _byteToHex[buf[15]];
    return guid;
}
