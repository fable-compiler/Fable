#[cfg(not(feature = "no_std"))]
pub mod File_ {
    use crate::String_::{fromString, string};

    pub fn exists(path: string) -> bool {
        std::path::Path::new(path.as_str()).exists()
    }

    pub fn writeAllText(path: string, contents: string) {
        std::fs::write(path.as_str(), contents.as_str())
            .unwrap_or_else(|e| panic!("Could not write file '{}': {}", path.as_str(), e))
    }

    pub fn readAllText(path: string) -> string {
        let contents = std::fs::read_to_string(path.as_str())
            .unwrap_or_else(|e| panic!("Could not read file '{}': {}", path.as_str(), e));
        fromString(contents)
    }

    pub fn delete(path: string) {
        // .NET File.Delete is a no-op when the file does not exist.
        match std::fs::remove_file(path.as_str()) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => panic!("Could not delete file '{}': {}", path.as_str(), e),
        }
    }
}
