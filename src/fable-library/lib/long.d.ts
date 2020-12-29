// Type definitions for long.js 4.0.0
// Project: https://github.com/dcodeIO/long.js
// Definitions by: Peter Kooijmans <https://github.com/peterkooijmans>
// Definitions: https://github.com/DefinitelyTyped/DefinitelyTyped
// Definitions by: Denis Cappellin <https://github.com/cappellin>
import { CustomNumeric } from "../Numeric";

export default Long;

export declare class Long implements CustomNumeric {
    /**
     * Constructs a 64 bit two's-complement integer, given its low and high 32 bit values as signed integers. See the from* functions below for more convenient ways of constructing Longs.
     */
    constructor(low: number, high?: number, unsigned?: boolean);

    /**
     * The high 32 bits as a signed value.
     */
    high: number;

    /**
     * The low 32 bits as a signed value.
     */
    low: number;

    /**
     * Whether unsigned or not.
     */
    unsigned: boolean;
}

/**
 * Maximum unsigned value.
 */
export const MAX_UNSIGNED_VALUE: Long;

/**
 * Maximum signed value.
 */
export const MAX_VALUE: Long;

/**
 * Minimum signed value.
 */
export const MIN_VALUE: Long;

/**
 * Signed negative one.
 */
export const NEG_ONE: Long;

/**
 * Signed one.
 */
export const ONE: Long;

/**
 * Unsigned one.
 */
export const UONE: Long;

/**
 * Unsigned zero.
 */
export const UZERO: Long;

/**
 * Signed zero
 */
export const ZERO: Long;

/**
 * Returns a Long representing the 64 bit integer that comes by concatenating the given low and high bits. Each is assumed to use 32 bits.
 */
export function fromBits(lowBits: number, highBits: number, unsigned?: boolean): Long;

/**
 * Returns a Long representing the given 32 bit integer value.
 */
export function fromInt(value: number, unsigned?: boolean): Long;

/**
 * Returns a Long representing the given value, provided that it is a finite number. Otherwise, zero is returned.
 */
export function fromNumber(value: number, unsigned?: boolean): Long;

/**
 * Returns a Long representation of the given string, written using the specified radix.
 */
export function fromString(str: string, unsigned?: boolean | number, radix?: number): Long;

/**
 * Creates a Long from its byte representation.
 */
export function fromBytes(bytes: number[], unsigned?: boolean, le?: boolean): Long;

/**
 * Creates a Long from its little endian byte representation.
 */
export function fromBytesLE(bytes: number[], unsigned?: boolean): Long;

/**
 * Creates a Long from its little endian byte representation.
 */
export function fromBytesBE(bytes: number[], unsigned?: boolean): Long;

/**
 * Tests if the specified object is a Long.
 */
export function isLong(obj: any): boolean;

/**
 * Converts the specified value to a Long.
 */
export function fromValue(val: Long | number | string | { low: number, high: number, unsigned: boolean }, unsigned?: boolean): Long;

/**
 * Returns the sum of this and the specified Long.
 */
export function add($this: Long, addend: number | Long | string): Long;

/**
 * Returns the bitwise AND of this Long and the specified.
 */
export function and($this: Long, other: Long | number | string): Long;

/**
 * Compares this Long's value with the specified's.
 */
export function compare($this: Long, other: Long | number | string): number;

/**
 * Compares this Long's value with the specified's.
 */
// export function comp($this: Long, other: Long | number | string): number;

/**
 * Returns this Long divided by the specified.
 */
export function divide($this: Long, divisor: Long | number | string): Long;

/**
 * Returns this Long divided by the specified.
 */
// export function div($this: Long, divisor: Long | number | string): Long;

/**
 * Tests if this Long's value equals the specified's.
 */
export function equals($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value equals the specified's.
 */
// export function eq($this: Long, other: Long | number | string): boolean;

/**
 * Gets the high 32 bits as a signed integer.
 */
export function getHighBits($this: Long): number;

/**
 * Gets the high 32 bits as an unsigned integer.
 */
export function getHighBitsUnsigned($this: Long): number;

/**
 * Gets the low 32 bits as a signed integer.
 */
export function getLowBits($this: Long): number;

/**
 * Gets the low 32 bits as an unsigned integer.
 */
export function getLowBitsUnsigned($this: Long): number;

/**
 * Gets the number of bits needed to represent the absolute value of this Long.
 */
export function getNumBitsAbs($this: Long): number;

/**
 * Tests if this Long's value is greater than the specified's.
 */
export function greaterThan($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value is greater than the specified's.
 */
// export function gt($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value is greater than or equal the specified's.
 */
export function greaterThanOrEqual($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value is greater than or equal the specified's.
 */
// export function gte($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value is even.
 */
export function isEven($this: Long): boolean;

/**
 * Tests if this Long's value is negative.
 */
export function isNegative($this: Long): boolean;

/**
 * Tests if this Long's value is odd.
 */
export function isOdd($this: Long): boolean;

/**
 * Tests if this Long's value is positive.
 */
export function isPositive($this: Long): boolean;

/**
 * Tests if this Long's value equals zero.
 */
export function isZero(): boolean;

/**
 * Tests if this Long's value is less than the specified's.
 */
export function lessThan($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value is less than the specified's.
 */
// export function lt($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value is less than or equal the specified's.
 */
export function lessThanOrEqual($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value is less than or equal the specified's.
 */
// export function lte($this: Long, other: Long | number | string): boolean;

/**
 * Returns this Long modulo the specified.
 */
export function modulo($this: Long, other: Long | number | string): Long;

/**
 * Returns this Long modulo the specified.
 */
// export function mod($this: Long, other: Long | number | string): Long;

/**
 * Returns the product of this and the specified Long.
 */
export function multiply($this: Long, multiplier: Long | number | string): Long;

/**
 * Returns the product of this and the specified Long.
 */
// export function mul($this: Long, multiplier: Long | number | string): Long;

/**
 * Negates this Long's value.
 */
export function negate($this: Long): Long;

/**
 * Negates this Long's value.
 */
// export function neg($this: Long): Long;

/**
 * Returns the bitwise NOT of this Long.
 */
export function not($this: Long): Long;

/**
 * Tests if this Long's value differs from the specified's.
 */
export function notEquals($this: Long, other: Long | number | string): boolean;

/**
 * Tests if this Long's value differs from the specified's.
 */
// export function neq($this: Long, other: Long | number | string): boolean;

/**
 * Returns the bitwise OR of this Long and the specified.
 */
export function or($this: Long, other: Long | number | string): Long;

/**
 * Returns this Long with bits shifted to the left by the given amount.
 */
export function shiftLeft($this: Long, numBits: number | Long): Long;

/**
 * Returns this Long with bits shifted to the left by the given amount.
 */
// export function shl($this: Long, numBits: number | Long): Long;

/**
 * Returns this Long with bits arithmetically shifted to the right by the given amount.
 */
export function shiftRight($this: Long, numBits: number | Long): Long;

/**
 * Returns this Long with bits arithmetically shifted to the right by the given amount.
 */
// export function shr($this: Long, numBits: number | Long): Long;

/**
 * Returns this Long with bits logically shifted to the right by the given amount.
 */
export function shiftRightUnsigned($this: Long, numBits: number | Long): Long;

/**
 * Returns this Long with bits logically shifted to the right by the given amount.
 */
// export function shru($this: Long, numBits: number | Long): Long;

/**
 * Returns the difference of this and the specified Long.
 */
export function subtract($this: Long, subtrahend: number | Long | string): Long;

/**
 * Returns the difference of this and the specified Long.
 */
// export function sub($this: Long, subtrahend: number | Long | string): Long;

/**
 * Converts the Long to a 32 bit integer, assuming it is a 32 bit integer.
 */
export function toInt($this: Long): number;

/**
 * Converts the Long to a the nearest floating-point representation of this value (double, 53 bit mantissa).
 */
export function toNumber($this: Long): number;

/**
 * Converts this Long to its byte representation.
 */

export function toBytes($this: Long, le?: boolean): number[];

/**
 * Converts this Long to its little endian byte representation.
 */

export function toBytesLE($this: Long): number[];

/**
 * Converts this Long to its big endian byte representation.
 */

export function toBytesBE($this: Long): number[];

/**
 * Converts this Long to signed.
 */
export function toSigned($this: Long): Long;

/**
 * Converts the Long to a string written in the specified radix.
 */
export function toString($this: Long, radix?: number): string;

/**
 * Converts this Long to unsigned.
 */
export function toUnsigned($this: Long): Long;

/**
 * Returns the bitwise XOR of this Long and the given one.
 */
export function xor($this: Long, other: Long | number | string): Long;

/**
 * Returns this Long with bits rotated to the left by the given amount.
 */
export function rotateLeft($this: Long, numBits: number): Long;

/**
 * Returns this Long with bits rotated to the right by the given amount.
 */
export function rotateRight($this: Long, numBits: number): Long;
