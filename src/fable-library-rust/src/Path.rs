#[cfg(not(feature = "no_std"))]
pub mod Path_ {
    use crate::String_::{fromString, string};

    fn is_sep(c: char) -> bool {
        c == '/' || c == '\\'
    }

    // Index just after the last path separator, or 0 if none.
    fn file_name_start(path: &str) -> usize {
        match path.rfind(is_sep) {
            Some(i) => i + 1,
            None => 0,
        }
    }

    // .NET/JS-style: returns the substring after the last separator.
    pub fn getFileName(path: string) -> string {
        let s = path.as_str();
        fromString(s[file_name_start(s)..].to_string())
    }

    // Returns the directory portion (everything before the last separator),
    // or an empty string when there is no separator. Trailing separators on
    // the directory portion are trimmed, matching .NET semantics.
    pub fn getDirectoryName(path: string) -> string {
        let s = path.as_str();
        match s.rfind(is_sep) {
            Some(i) => {
                let dir = &s[..i];
                let dir = dir.trim_end_matches(is_sep);
                fromString(dir.to_string())
            }
            None => string(""),
        }
    }

    // Returns the extension including the leading dot, or an empty string.
    // Matches Node's extname/.NET: a leading dot in the file name is not an
    // extension (e.g. ".gitignore" has no extension).
    pub fn getExtension(path: string) -> string {
        let s = path.as_str();
        let name = &s[file_name_start(s)..];
        match name.rfind('.') {
            Some(i) if i > 0 => fromString(name[i..].to_string()),
            _ => string(""),
        }
    }

    pub fn getFileNameWithoutExtension(path: string) -> string {
        let s = path.as_str();
        let name = &s[file_name_start(s)..];
        let stem = match name.rfind('.') {
            Some(i) if i > 0 => &name[..i],
            _ => name,
        };
        fromString(stem.to_string())
    }

    pub fn hasExtension(path: string) -> bool {
        let s = path.as_str();
        let name = &s[file_name_start(s)..];
        matches!(name.rfind('.'), Some(i) if i > 0)
    }

    fn combine_str(a: &str, b: &str) -> String {
        if a.is_empty() {
            return b.to_string();
        }
        if b.is_empty() {
            return a.to_string();
        }
        // .NET semantics: a rooted later argument discards the earlier one,
        // e.g. Path.Combine("/foo", "/bar") == "/bar".
        if b.starts_with(is_sep) {
            return b.to_string();
        }
        let a = a.trim_end_matches(is_sep);
        format!("{}/{}", a, b)
    }

    pub fn combine2(path1: string, path2: string) -> string {
        fromString(combine_str(path1.as_str(), path2.as_str()))
    }

    pub fn combine3(path1: string, path2: string, path3: string) -> string {
        let ab = combine_str(path1.as_str(), path2.as_str());
        fromString(combine_str(&ab, path3.as_str()))
    }

    pub fn getTempPath() -> string {
        fromString(std::env::temp_dir().to_string_lossy().into_owned())
    }

    // Non-cryptographic randomness derived from the system clock and an
    // atomic counter; good enough for unique temp file names.
    fn rand_u64() -> u64 {
        use core::sync::atomic::{AtomicU64, Ordering};
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(0);
        let c = COUNTER.fetch_add(1, Ordering::Relaxed);
        let mut x = nanos ^ c.wrapping_mul(0x9E37_79B9_7F4A_7C15);
        x ^= x >> 30;
        x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
        x ^= x >> 27;
        x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
        x ^= x >> 31;
        x
    }

    // Mirrors .NET's Path.GetRandomFileName(): 8 random hex chars, a dot,
    // then 3 more random hex chars.
    pub fn getRandomFileName() -> string {
        let name = (rand_u64() as u32) & 0xFFFF_FFFF;
        let ext = (rand_u64() as u32) & 0xFFF;
        fromString(format!("{:08x}.{:03x}", name, ext))
    }
}
