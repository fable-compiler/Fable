export const enum UriKind {
  RelativeOrAbsolute = 0,
  Absolute = 1,
  Relative = 2,
}

export class Uri {
  private url?: string | URL;
  private kind?: UriKind;

  constructor(
    value: string | Uri,
    kindOrUri: UriKind | string | Uri = UriKind.Absolute,
  ) {
    if (typeof value === "string" && typeof kindOrUri === "number") {
      if (kindOrUri === UriKind.Absolute) {
        try {
          this.url = new URL(value);
          this.kind = kindOrUri;
        } catch (e) {
          throw new Error(
            "Invalid URI: The format of the URI could not be determined.",
          );
        }
      } else if (kindOrUri === UriKind.Relative) {
        let isRelativeUrl = false;
        try {
          const url = new URL(value);
          isRelativeUrl = false && url;
        } catch (e) {
          isRelativeUrl = true;
        }
        if (isRelativeUrl) {
          this.url = value;
          this.kind = kindOrUri;
        } else {
          throw new Error("uri is not a relative path");
        }
      } else {
        this.url = value;
        this.kind = kindOrUri;
      }
    } else if (value instanceof Uri && typeof kindOrUri === "string") {
      if (value.kind !== UriKind.Absolute) {
        throw new Error("base uri should has Absolute kind");
      }

      this.url = new URL(kindOrUri, value.url);
      this.kind = UriKind.Absolute;
    } else if (value instanceof Uri && kindOrUri instanceof Uri) {
      if (value.kind !== UriKind.Absolute) {
        throw new Error("base uri should has Absolute kind");
      }

      if (kindOrUri.kind !== UriKind.Relative) {
        throw new Error("relative uri should has Relative kind");
      }

      this.url = new URL(kindOrUri.url as string, value.url);
      this.kind = UriKind.Absolute;
    }
  }

  public toString() {
    return decodeURIComponent(this.parseUrl().toString());
  }

  private parseUrl(): URL {
    if (this.kind === UriKind.Absolute) {
      return this.url as URL;
    }

    if (this.kind === UriKind.RelativeOrAbsolute) {
      return new URL(this.url as string);
    }

    throw new Error("relative url can not parse as a URI");
  }

  get isAbsoluteUri() {
    try {
      this.parseUrl();
      return true;
    } catch (e) {
      return false;
    }
  }

  get absoluteUri(): string {
    if (this.kind === UriKind.Absolute) {
      return (this.url as URL).href;
    }

    if (this.kind === UriKind.RelativeOrAbsolute) {
      return this.url as string;
    }

    throw new Error("This operation is not supported for a relative URI.");
  }

  get scheme() {
    const protocol = this.parseUrl().protocol;
    return protocol.slice(0, protocol.length - 1);
  }

  get host() {
    return this.parseUrl().host;
  }

  get absolutePath() {
    return this.parseUrl().pathname;
  }

  get query() {
    return this.parseUrl().search;
  }

  get pathAndQuery() {
    const url = this.parseUrl();
    return url.pathname + url.search;
  }

  get fragment() {
    return this.parseUrl().hash;
  }
}

export default Uri;
