#[cfg_attr(rustfmt, rustfmt::skip)]
pub mod Uri_ {
    use crate::Native_::{format, LrcPtr, MutCell, String, ToString, Vec};
    use crate::String_::{fromSlice, fromString, string};

    // UriKind: 0 = RelativeOrAbsolute, 1 = Absolute, 2 = Relative

    #[derive(Clone, Default, PartialEq)]
    pub struct Uri {
        isAbsolute: bool,
        hasAuthority: bool,  // true for scheme://authority/... , false for scheme:path (mailto:, tel:, urn:)
        original: string,
        relative: string,    // raw string for relative URIs
        schemePart: string,
        hostPart: string,
        port: i32,           // -1 when no explicit port
        pathPart: string,
        queryPart: string,   // "" or "?..."
        fragmentPart: string, // "" or "#..."
    }

    // -------------------------------------------------------------------
    // helpers
    // -------------------------------------------------------------------

    // Returns the byte index of the ':' that terminates a valid URI scheme
    // prefix (scheme = ALPHA *(ALPHA / DIGIT / "+" / "-" / ".")), or None when
    // `s` has no scheme (i.e. it is a relative reference). This recognizes both
    // `scheme://authority/...` and authority-less forms like `mailto:a@b.com`.
    fn scheme_end(s: &str) -> Option<usize> {
        let bytes = s.as_bytes();
        match bytes.first() {
            Some(&c) if (c as char).is_ascii_alphabetic() => {}
            _ => return None,
        }
        let mut i = 1;
        while i < bytes.len() {
            let c = bytes[i] as char;
            if c == ':' {
                return Some(i);
            } else if c.is_ascii_alphanumeric() || c == '+' || c == '-' || c == '.' {
                i += 1;
            } else {
                return None;
            }
        }
        None
    }

    fn is_absolute_str(s: &str) -> bool {
        scheme_end(s).is_some()
    }

    // RFC 3986 §5.2.4 "remove dot segments": collapses `.` and `..` in a path.
    fn remove_dot_segments(path: &str) -> String {
        let mut input = path.to_string();
        let mut output = String::new();
        while !input.is_empty() {
            if input.starts_with("../") {
                input.replace_range(0..3, "");
            } else if input.starts_with("./") {
                input.replace_range(0..2, "");
            } else if input.starts_with("/./") {
                input.replace_range(0..3, "/");
            } else if input == "/." {
                input = "/".to_string();
            } else if input.starts_with("/../") {
                input.replace_range(0..4, "/");
                remove_last_segment(&mut output);
            } else if input == "/.." {
                input = "/".to_string();
                remove_last_segment(&mut output);
            } else if input == "." || input == ".." {
                input.clear();
            } else {
                let start = if input.starts_with('/') { 1 } else { 0 };
                let end = match input[start..].find('/') {
                    Some(i) => start + i,
                    None => input.len(),
                };
                output.push_str(&input[..end]);
                input.replace_range(0..end, "");
            }
        }
        output
    }

    fn remove_last_segment(output: &mut String) {
        match output.rfind('/') {
            Some(i) => output.truncate(i),
            None => output.clear(),
        }
    }

    fn is_default_port(scheme: &str, port: i32) -> bool {
        port == -1
            || (scheme == "http" && port == 80)
            || (scheme == "https" && port == 443)
    }

    fn authority_str(scheme: &str, host: &str, port: i32) -> String {
        if is_default_port(scheme, port) {
            host.to_string()
        } else {
            format!("{}:{}", host, port)
        }
    }

    fn hex_val(b: u8) -> Option<u8> {
        match b {
            b'0'..=b'9' => Some(b - b'0'),
            b'a'..=b'f' => Some(b - b'a' + 10),
            b'A'..=b'F' => Some(b - b'A' + 10),
            _ => None,
        }
    }

    fn percent_decode(s: &str) -> String {
        let bytes = s.as_bytes();
        let mut out: Vec<u8> = Vec::new();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'%' && i + 2 < bytes.len() {
                if let (Some(h), Some(l)) = (hex_val(bytes[i + 1]), hex_val(bytes[i + 2])) {
                    out.push(h * 16 + l);
                    i += 3;
                    continue;
                }
            }
            out.push(bytes[i]);
            i += 1;
        }
        String::from_utf8_lossy(&out).into_owned()
    }

    // parse scheme://host:port/path?query#fragment , or the authority-less
    // form scheme:path?query#fragment (mailto:, tel:, urn:, ...).
    // Returns (scheme, hasAuthority, host, port, path, query, fragment).
    fn parse_absolute(s: &str) -> (String, bool, String, i32, String, String, String) {
        let idx = scheme_end(s).unwrap();
        let scheme = s[..idx].to_ascii_lowercase();
        let after = &s[idx + 1..]; // everything after "scheme:"

        match after.strip_prefix("//") {
            Some(rest) => {
                // scheme://authority/path?query#fragment
                let auth_end = rest.find(|c| c == '/' || c == '?' || c == '#').unwrap_or(rest.len());
                let authority = &rest[..auth_end];
                let remainder = &rest[auth_end..];

                let (host, port) = match authority.rfind(':') {
                    Some(ci) => {
                        let h = &authority[..ci];
                        let p = &authority[ci + 1..];
                        match p.parse::<i32>() {
                            Ok(n) => (h.to_ascii_lowercase(), n),
                            Err(_) => (authority.to_ascii_lowercase(), -1),
                        }
                    }
                    None => (authority.to_ascii_lowercase(), -1),
                };

                let (before_frag, fragment) = match remainder.find('#') {
                    Some(fi) => (&remainder[..fi], remainder[fi..].to_string()),
                    None => (remainder, String::new()),
                };
                let (path, query) = match before_frag.find('?') {
                    Some(qi) => (before_frag[..qi].to_string(), before_frag[qi..].to_string()),
                    None => (before_frag.to_string(), String::new()),
                };
                let path = if path.is_empty() { "/".to_string() } else { path };
                (scheme, true, host, port, path, query, fragment)
            }
            None => {
                // authority-less: scheme:path?query#fragment (no host/port)
                let (before_frag, fragment) = match after.find('#') {
                    Some(fi) => (&after[..fi], after[fi..].to_string()),
                    None => (after, String::new()),
                };
                let (path, query) = match before_frag.find('?') {
                    Some(qi) => (before_frag[..qi].to_string(), before_frag[qi..].to_string()),
                    None => (before_frag.to_string(), String::new()),
                };
                (scheme, false, String::new(), -1, path, query, fragment)
            }
        }
    }

    fn parse_abs_uri(orig: &str) -> Uri {
        let (scheme, has_authority, host, port, path, query, fragment) = parse_absolute(orig);
        Uri {
            isAbsolute: true,
            hasAuthority: has_authority,
            original: fromSlice(orig),
            relative: string(""),
            schemePart: fromString(scheme),
            hostPart: fromString(host),
            port,
            pathPart: fromString(path),
            queryPart: fromString(query),
            fragmentPart: fromString(fragment),
        }
    }

    fn make_relative(orig: &str) -> Uri {
        Uri {
            isAbsolute: false,
            hasAuthority: false,
            original: fromSlice(orig),
            relative: fromSlice(orig),
            schemePart: string(""),
            hostPart: string(""),
            port: -1,
            pathPart: string(""),
            queryPart: string(""),
            fragmentPart: string(""),
        }
    }

    // resolve a relative reference against an absolute base URI
    fn resolve(base: &Uri, rel: &str) -> Uri {
        if is_absolute_str(rel) {
            return parse_abs_uri(rel);
        }
        let (before_frag, fragment) = match rel.find('#') {
            Some(fi) => (&rel[..fi], rel[fi..].to_string()),
            None => (rel, String::new()),
        };
        let (rel_path, query) = match before_frag.find('?') {
            Some(qi) => (before_frag[..qi].to_string(), before_frag[qi..].to_string()),
            None => (before_frag.to_string(), String::new()),
        };
        let base_path = base.pathPart.as_str();
        let merged_path = if rel_path.is_empty() {
            base_path.to_string()
        } else if rel_path.starts_with('/') {
            rel_path
        } else {
            let cut = base_path.rfind('/').map(|i| i + 1).unwrap_or(0);
            format!("{}{}", &base_path[..cut], rel_path)
        };
        // RFC 3986 §5.2.4: collapse `.`/`..` segments in the merged path.
        let new_path = remove_dot_segments(&merged_path);
        let scheme = base.schemePart.as_str();
        let host = base.hostPart.as_str();
        let auth = authority_str(scheme, host, base.port);
        let orig = format!("{}://{}{}{}{}", scheme, auth, new_path, query, fragment);
        Uri {
            isAbsolute: true,
            hasAuthority: base.hasAuthority,
            original: fromString(orig),
            relative: string(""),
            schemePart: base.schemePart.clone(),
            hostPart: base.hostPart.clone(),
            port: base.port,
            pathPart: fromString(new_path),
            queryPart: fromString(query),
            fragmentPart: fromString(fragment),
        }
    }

    fn build_from_string(orig: &str, kind: i32) -> Result<Uri, &'static str> {
        let is_abs = is_absolute_str(orig);
        match kind {
            1 => {
                // Absolute
                if is_abs {
                    Ok(parse_abs_uri(orig))
                } else {
                    Err("Invalid URI: The format of the URI could not be determined.")
                }
            }
            2 => {
                // Relative
                if is_abs {
                    Err("URI is not a relative path.")
                } else {
                    Ok(make_relative(orig))
                }
            }
            _ => {
                // RelativeOrAbsolute
                if is_abs {
                    Ok(parse_abs_uri(orig))
                } else {
                    Ok(make_relative(orig))
                }
            }
        }
    }

    // -------------------------------------------------------------------
    // instance surface
    // -------------------------------------------------------------------

    impl Uri {
        fn ensure_absolute(&self) {
            if !self.isAbsolute {
                panic!("This operation is not supported for a relative URI.");
            }
        }

        fn build_absolute(&self) -> String {
            let scheme = self.schemePart.as_str();
            if self.hasAuthority {
                let auth = authority_str(scheme, self.hostPart.as_str(), self.port);
                format!(
                    "{}://{}{}{}{}",
                    scheme,
                    auth,
                    self.pathPart.as_str(),
                    self.queryPart.as_str(),
                    self.fragmentPart.as_str()
                )
            } else {
                // authority-less scheme (mailto:, tel:, urn:): no `//host`.
                format!(
                    "{}:{}{}{}",
                    scheme,
                    self.pathPart.as_str(),
                    self.queryPart.as_str(),
                    self.fragmentPart.as_str()
                )
            }
        }

        pub fn isAbsoluteUri(&self) -> bool {
            self.isAbsolute
        }

        pub fn scheme(&self) -> string {
            self.ensure_absolute();
            self.schemePart.clone()
        }

        pub fn host(&self) -> string {
            self.ensure_absolute();
            self.hostPart.clone()
        }

        pub fn absolutePath(&self) -> string {
            self.ensure_absolute();
            self.pathPart.clone()
        }

        pub fn query(&self) -> string {
            self.ensure_absolute();
            self.queryPart.clone()
        }

        pub fn fragment(&self) -> string {
            self.ensure_absolute();
            self.fragmentPart.clone()
        }

        pub fn pathAndQuery(&self) -> string {
            self.ensure_absolute();
            fromString(format!("{}{}", self.pathPart.as_str(), self.queryPart.as_str()))
        }

        pub fn absoluteUri(&self) -> string {
            self.ensure_absolute();
            fromString(self.build_absolute())
        }

        pub fn originalString(&self) -> string {
            self.original.clone()
        }
    }

    impl core::fmt::Display for Uri {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            if self.isAbsolute {
                write!(f, "{}", percent_decode(&self.build_absolute()))
            } else {
                write!(f, "{}", self.relative.as_str())
            }
        }
    }

    impl core::fmt::Debug for Uri {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            core::fmt::Display::fmt(self, f)
        }
    }

    // -------------------------------------------------------------------
    // constructors (free functions, mirror System.Uri overloads)
    // -------------------------------------------------------------------

    pub fn create(uriString: string) -> LrcPtr<Uri> {
        createWithKind(uriString, 1)
    }

    pub fn createWithKind(uriString: string, kind: i32) -> LrcPtr<Uri> {
        match build_from_string(uriString.as_str(), kind) {
            Ok(u) => LrcPtr::new(u),
            Err(msg) => panic!("{}", msg),
        }
    }

    pub fn createFromString(baseUri: LrcPtr<Uri>, relativeUri: string) -> LrcPtr<Uri> {
        if !baseUri.isAbsolute {
            panic!("Invalid URI: A base URI cannot be created because the 'baseUri' parameter represents a relative URI.");
        }
        LrcPtr::new(resolve(&baseUri, relativeUri.as_str()))
    }

    pub fn createFromUri(baseUri: LrcPtr<Uri>, relativeUri: LrcPtr<Uri>) -> LrcPtr<Uri> {
        if !baseUri.isAbsolute {
            panic!("Invalid URI: A base URI cannot be created because the 'baseUri' parameter represents a relative URI.");
        }
        if relativeUri.isAbsolute {
            // .NET: when the second URI is absolute it wins, and the base is ignored.
            relativeUri.clone()
        } else {
            LrcPtr::new(resolve(&baseUri, relativeUri.relative.as_str()))
        }
    }

    pub fn tryCreateWithKind(uriString: string, kind: i32, res: &MutCell<LrcPtr<Uri>>) -> bool {
        match build_from_string(uriString.as_str(), kind) {
            Ok(u) => {
                res.set(LrcPtr::new(u));
                true
            }
            Err(_) => false,
        }
    }

    pub fn tryCreateFromUri(
        baseUri: LrcPtr<Uri>,
        relativeUri: LrcPtr<Uri>,
        res: &MutCell<LrcPtr<Uri>>,
    ) -> bool {
        if !baseUri.isAbsolute {
            return false;
        }
        if relativeUri.isAbsolute {
            // .NET: when the second URI is absolute it wins, and the base is ignored.
            res.set(relativeUri.clone());
        } else {
            res.set(LrcPtr::new(resolve(&baseUri, relativeUri.relative.as_str())));
        }
        true
    }

    // --- Percent-encoding (RFC 3986). Encoding operates on UTF-8 bytes. ---

    fn is_unreserved(b: u8) -> bool {
        b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.' | b'~')
    }

    // gen-delims and sub-delims, kept intact by EscapeUriString.
    fn is_reserved(b: u8) -> bool {
        matches!(
            b,
            b':' | b'/' | b'?' | b'#' | b'[' | b']' | b'@'
            | b'!' | b'$' | b'&' | b'\'' | b'(' | b')' | b'*' | b'+' | b',' | b';' | b'='
        )
    }

    fn escape_with(s: &str, keep: fn(u8) -> bool) -> string {
        const HEX: &[u8; 16] = b"0123456789ABCDEF";
        let mut out = String::with_capacity(s.len());
        for b in s.bytes() {
            if keep(b) {
                out.push(b as char);
            } else {
                out.push('%');
                out.push(HEX[(b >> 4) as usize] as char);
                out.push(HEX[(b & 0x0F) as usize] as char);
            }
        }
        fromString(out)
    }

    pub fn escapeDataString(s: string) -> string {
        escape_with(s.as_str(), is_unreserved)
    }

    pub fn escapeUriString(s: string) -> string {
        fn keep(b: u8) -> bool {
            is_unreserved(b) || is_reserved(b)
        }
        escape_with(s.as_str(), keep)
    }

    pub fn unescapeDataString(s: string) -> string {
        let bytes = s.as_str().as_bytes();
        let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
        let mut i = 0;
        while i < bytes.len() {
            // An invalid or truncated %XX sequence is left untouched, as in .NET.
            if bytes[i] == b'%' && i + 2 < bytes.len() {
                if let (Some(hi), Some(lo)) = (hex_val(bytes[i + 1]), hex_val(bytes[i + 2])) {
                    out.push((hi << 4) | lo);
                    i += 3;
                    continue;
                }
            }
            out.push(bytes[i]);
            i += 1;
        }
        fromString(String::from_utf8_lossy(&out).into_owned())
    }
}
