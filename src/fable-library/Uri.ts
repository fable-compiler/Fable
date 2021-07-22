import { FSharpRef } from "./Types";

export const enum UriKind {
  RelativeOrAbsolute = 0,
  Absolute = 1,
  Relative = 2,
}

type State =
  | { value: URL; kind: UriKind.Absolute }
  | { value: string; kind: UriKind.Relative | UriKind.RelativeOrAbsolute };

type Result<T> = { tag: "ok"; value: T } | { tag: "error"; error: string };

const ok = <T>(value: T): Result<T> => ({
  tag: "ok",
  value,
});

const error = <T>(error: string): Result<T> => ({ tag: "error", error });

export class Uri {
  private uri: State;

  private constructor(state: State) {
    this.uri = state;
  }

  private static isAbsoluteUri(uri: string): boolean {
    try {
      // tslint:disable-next-line no-unused-expression
      new URL(uri);
      return true;
    } catch {
      return false;
    }
  }

  private static tryCreateWithKind(uri: string, kind: UriKind): Result<Uri> {
    switch (kind) {
      case UriKind.Absolute:
        try {
          return ok(new Uri({ value: new URL(uri), kind }));
        } catch {
          return error(
            "Invalid URI: The format of the URI could not be determined."
          );
        }
      case UriKind.Relative:
        return Uri.isAbsoluteUri(uri)
          ? error("URI is not a relative path.")
          : ok(new Uri({ value: uri, kind }));
      case UriKind.RelativeOrAbsolute:
        return ok(new Uri({ value: uri, kind }));
      default:
        const never: never = kind;
        return never;
    }
  }

  private static tryCreateWithBase(
    baseUri: Uri,
    relativeUri: string | Uri
  ): Result<Uri> {
    return baseUri.uri.kind !== UriKind.Absolute
      ? error("Base URI should have Absolute kind")
      : typeof relativeUri === "string"
      ? ok(
          new Uri({
            value: new URL(relativeUri, baseUri.uri.value),
            kind: UriKind.Absolute,
          })
        )
      : relativeUri.uri.kind !== UriKind.Relative
      ? error("Relative URI should have Relative kind")
      : ok(
          new Uri({
            value: new URL(relativeUri.uri.value, baseUri.uri.value),
            kind: UriKind.Absolute,
          })
        );
  }

  private static tryCreateImpl(
    value: string | Uri,
    kindOrUri: UriKind | string | Uri = UriKind.Absolute
  ): Result<Uri> {
    return typeof value === "string"
      ? typeof kindOrUri !== "number"
        ? error("Kind must be specified when the baseUri is a string.")
        : Uri.tryCreateWithKind(value, kindOrUri)
      : typeof kindOrUri === "number"
      ? error(
          "Kind should not be specified when the baseUri is an absolute Uri."
        )
      : Uri.tryCreateWithBase(value, kindOrUri);
  }

  public static create(baseUri: Uri, relativeUri: string | Uri): Uri;
  public static create(uriString: string, uriKind: UriKind): Uri;
  public static create(
    value: string | Uri,
    kindOrUri: UriKind | string | Uri = UriKind.Absolute
  ): Uri {
    const result = Uri.tryCreateImpl(value, kindOrUri);
    switch (result.tag) {
      case "ok":
        return result.value;
      case "error":
        throw new Error(result.error);
      default:
        const never: never = result;
        return never;
    }
  }

  public static tryCreate(
    baseUri: Uri,
    relativeUri: string | Uri,
    result: FSharpRef<Uri>
  ): boolean;
  public static tryCreate(
    uriString: string,
    uriKind: UriKind,
    result: FSharpRef<Uri>
  ): boolean;
  public static tryCreate(
    value: string | Uri,
    kindOrUri: UriKind | string | Uri = UriKind.Absolute,
    out: FSharpRef<Uri>
  ): boolean {
    const result = Uri.tryCreateImpl(value, kindOrUri);
    switch (result.tag) {
      case "ok":
        out.contents = result.value;
        return true;
      case "error":
        return false;
      default:
        const never: never = result;
        return never;
    }
  }

  public toString() {
    return decodeURIComponent(this.asUrl().toString());
  }

  private asUrl(): URL {
    switch (this.uri.kind) {
      case UriKind.Absolute:
        return this.uri.value;
      case UriKind.RelativeOrAbsolute:
        return new URL(this.uri.value);
      case UriKind.Relative:
        throw new Error("relative url can not parse as a URI");
      default:
        const never: never = this.uri;
        return never;
    }
  }

  get isAbsoluteUri() {
    try {
      this.asUrl();
      return true;
    } catch {
      return false;
    }
  }

  get absoluteUri(): string {
    return this.asUrl().href;
  }

  get scheme() {
    const protocol = this.asUrl().protocol;
    return protocol.slice(0, protocol.length - 1);
  }

  get host() {
    return this.asUrl().host;
  }

  get absolutePath() {
    return this.asUrl().pathname;
  }

  get query() {
    return this.asUrl().search;
  }

  get pathAndQuery() {
    const url = this.asUrl();
    return url.pathname + url.search;
  }

  get fragment() {
    return this.asUrl().hash;
  }
}

export default Uri;
