import { FSharpRef } from "./Types.js";
export declare const enum UriKind {
    RelativeOrAbsolute = 0,
    Absolute = 1,
    Relative = 2
}
export declare class Uri {
    private uri;
    private constructor();
    private static isAbsoluteUri;
    private static tryCreateWithKind;
    private static tryCreateWithBase;
    private static tryCreateImpl;
    static create(value: string | Uri, kindOrUri?: UriKind | string | Uri): Uri;
    static tryCreate(baseUri: Uri, relativeUri: string | Uri, result: FSharpRef<Uri>): boolean;
    static tryCreate(uriString: string, uriKind: UriKind, result: FSharpRef<Uri>): boolean;
    toString(): string;
    private asUrl;
    get isAbsoluteUri(): boolean;
    get absoluteUri(): string;
    get scheme(): string;
    get host(): string;
    get absolutePath(): string;
    get query(): string;
    get pathAndQuery(): string;
    get fragment(): string;
    get originalString(): string;
}
export default Uri;
