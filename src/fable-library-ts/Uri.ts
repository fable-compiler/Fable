import { FSharpRef, Result } from "./Types.js";

export const enum UriKind {
  RelativeOrAbsolute = 0,
  Absolute = 1,
  Relative = 2,
}

type State =
  | { original: string; value: URL; kind: UriKind.Absolute }
  | { original: string; value: string; kind: UriKind.Relative };

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
      new URL(uri);
      return true;
    } catch {
      return false;
    }
  }

  private static tryCreateWithKind(uri: string, kind: UriKind): Result<Uri> {
    switch (kind) {
      case UriKind.Absolute:
        return Uri.isAbsoluteUri(uri)
          ? ok(new Uri({ original: uri, value: new URL(uri), kind }))
          : error(
              "Invalid URI: The format of the URI could not be determined."
            );
      case UriKind.Relative:
        return Uri.isAbsoluteUri(uri)
          ? error("URI is not a relative path.")
          : ok(new Uri({ original: uri, value: uri, kind }));
      case UriKind.RelativeOrAbsolute:
        return Uri.isAbsoluteUri(uri)
          ? ok(new Uri({ original: uri, value: new URL(uri), kind: UriKind.Absolute }))
          : ok(new Uri({ original: uri, value: uri, kind: UriKind.Relative }));
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
            original: new URL(relativeUri, baseUri.uri.value).toString(),
            value: new URL(relativeUri, baseUri.uri.value),
            kind: UriKind.Absolute,
          })
        )
      : relativeUri.uri.kind === UriKind.Relative
      ? ok(
          new Uri({
            original: new URL(relativeUri.uri.value, baseUri.uri.value).toString(),
            value: new URL(relativeUri.uri.value, baseUri.uri.value),
            kind: UriKind.Absolute,
          })
        )
      : ok(baseUri);
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
    switch (this.uri.kind) {
      case UriKind.Absolute:
        return decodeURIComponent(this.asUrl().toString());
      case UriKind.Relative:
        return this.uri.value;
      default:
        const never: never = this.uri;
        return never;
    }
  }

  private asUrl(): URL {
    switch (this.uri.kind) {
      case UriKind.Absolute:
        return this.uri.value;
      case UriKind.Relative:
        throw new Error("This operation is not supported for a relative URI.");
      default:
        const never: never = this.uri;
        return never;
    }
  }

  get isAbsoluteUri() {
    return this.uri.kind === UriKind.Absolute;
  }

  get absoluteUri(): string {
    return this.asUrl().href;
  }

  get scheme() {
    const protocol = this.asUrl().protocol;
    return protocol.slice(0, protocol.length - 1);
  }

  get host() {
    const host = this.asUrl().host;

    if (host.includes(":")) {
      return host.split(":")[0];
    } else {
      return host;
    }
  }

  get absolutePath() {
    return this.asUrl().pathname;
  }

  get query() {
    return this.asUrl().search;
  }

  get isDefaultPort() {
    return this.port === 80;
  }

  get port() {
    const port = this.asUrl().port;

    if (port === "") {
      return 80;
    } else {
      return parseInt(port);
    }
  }

  get pathAndQuery() {
    const url = this.asUrl();
    return url.pathname + url.search;
  }

  get fragment() {
    return this.asUrl().hash;
  }

  get originalString(): string {
    return this.uri.original;
  }
}

export default Uri;
