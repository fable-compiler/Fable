import { class_type } from "./Reflection.js";
import { fromFloat64, op_Addition, toInt32, toFloat64, compare, fromInt32, toInt64 } from "./BigInt.js";
import { fill } from "./Array.js";
function Native_random() {
    return Math.random();
}
function Native_randomNext(min, max) {
    if (max < min) {
        throw new Error("minValue must be less than maxValue");
    }
    return Math.floor(Math.random() * (max - min)) + min;
}
function Native_randomBytes(buffer) {
    if (buffer == null) {
        throw new Error("Buffer cannot be null");
    }
    for (let i = 0; i < buffer.length; i += 6) {
        // Pick random 48-bit number. Fill buffer in 2 24-bit chunks to avoid bitwise truncation.
        let r = Math.floor(Math.random() * 281474976710656); // Low 24 bits = chunk 1.
        const rhi = Math.floor(r / 16777216); // High 24 bits shifted via division = chunk 2.
        for (let j = 0; j < 6 && i + j < buffer.length; j++) {
            if (j === 3) {
                r = rhi;
            }
            buffer[i + j] = r & 255;
            r >>>= 8;
        }
    }
    ;
}
export class NonSeeded {
    constructor() {
    }
    Next0() {
        return Native_randomNext(0, 2147483647);
    }
    Next1(maxValue) {
        return Native_randomNext(0, maxValue);
    }
    Next2(minValue, maxValue) {
        return Native_randomNext(minValue, maxValue);
    }
    NextDouble() {
        return Native_random();
    }
    NextBytes(buffer) {
        Native_randomBytes(buffer);
    }
}
export function NonSeeded_$reflection() {
    return class_type("Random.NonSeeded", void 0, NonSeeded);
}
export function NonSeeded_$ctor() {
    return new NonSeeded();
}
export class Seeded {
    constructor(seed) {
        this.MBIG = 2147483647;
        this.inext = 0;
        this.inextp = 0;
        this.seedArray = fill(new Array(56), 0, 56, 0);
        let ii = 0;
        let mj = 0;
        let mk = 0;
        const subtraction = ((seed === -2147483648) ? 2147483647 : Math.abs(seed)) | 0;
        mj = ((161803398 - subtraction) | 0);
        this.seedArray[55] = (mj | 0);
        mk = 1;
        for (let i = 1; i <= 54; i++) {
            ii = (((21 * i) % 55) | 0);
            this.seedArray[ii] = (mk | 0);
            mk = ((mj - mk) | 0);
            if (mk < 0) {
                mk = ((mk + this.MBIG) | 0);
            }
            mj = (this.seedArray[ii] | 0);
        }
        for (let k = 1; k <= 4; k++) {
            for (let i_1 = 1; i_1 <= 55; i_1++) {
                this.seedArray[i_1] = ((this.seedArray[i_1] - this.seedArray[1 + ((i_1 + 30) % 55)]) | 0);
                if (this.seedArray[i_1] < 0) {
                    this.seedArray[i_1] = ((this.seedArray[i_1] + this.MBIG) | 0);
                }
            }
        }
        this.inext = 0;
        this.inextp = 21;
    }
    Next0() {
        const this$ = this;
        return Seeded__InternalSample(this$) | 0;
    }
    Next1(maxValue) {
        const this$ = this;
        if (maxValue < 0) {
            throw new Error("maxValue must be positive");
        }
        return ~~(Seeded__Sample(this$) * maxValue) | 0;
    }
    Next2(minValue, maxValue) {
        const this$ = this;
        if (minValue > maxValue) {
            throw new Error("minValue must be less than maxValue");
        }
        const range = toInt64(fromInt32(maxValue - minValue));
        return ((compare(range, toInt64(fromInt32(2147483647))) <= 0) ? (~~(Seeded__Sample(this$) * toFloat64(range)) + minValue) : ~~toInt32(toInt64(op_Addition(toInt64(fromFloat64(Seeded__GetSampleForLargeRange(this$) * toFloat64(range))), toInt64(fromInt32(minValue)))))) | 0;
    }
    NextDouble() {
        const this$ = this;
        return Seeded__Sample(this$);
    }
    NextBytes(buffer) {
        const this$ = this;
        if (buffer == null) {
            throw new Error("buffer");
        }
        for (let i = 0; i <= (buffer.length - 1); i++) {
            buffer[i] = ((Seeded__InternalSample(this$) % (~~255 + 1)) & 0xFF);
        }
    }
}
export function Seeded_$reflection() {
    return class_type("Random.Seeded", void 0, Seeded);
}
export function Seeded_$ctor_Z524259A4(seed) {
    return new Seeded(seed);
}
function Seeded__InternalSample(_) {
    let retVal = 0;
    let locINext = _.inext;
    let locINextp = _.inextp;
    locINext = ((locINext + 1) | 0);
    if (locINext >= 56) {
        locINext = 1;
    }
    locINextp = ((locINextp + 1) | 0);
    if (locINextp >= 56) {
        locINextp = 1;
    }
    retVal = ((_.seedArray[locINext] - _.seedArray[locINextp]) | 0);
    if (retVal === _.MBIG) {
        retVal = ((retVal - 1) | 0);
    }
    if (retVal < 0) {
        retVal = ((retVal + _.MBIG) | 0);
    }
    _.seedArray[locINext] = (retVal | 0);
    _.inext = (locINext | 0);
    _.inextp = (locINextp | 0);
    return retVal | 0;
}
export function Seeded__Sample(this$) {
    return Seeded__InternalSample(this$) * (1 / this$.MBIG);
}
export function Seeded__GetSampleForLargeRange(this$) {
    let result = Seeded__InternalSample(this$);
    if ((Seeded__InternalSample(this$) % 2) === 0) {
        result = -result;
    }
    let d = result;
    d = (d + (2147483647 - 1));
    d = (d / (2 * ((2147483647 - 1) >>> 0)));
    return d;
}
export function nonSeeded() {
    return NonSeeded_$ctor();
}
export function seeded(seed) {
    return Seeded_$ctor_Z524259A4(seed);
}
