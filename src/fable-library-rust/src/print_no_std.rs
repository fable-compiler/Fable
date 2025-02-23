// no_std console output
const STDOUT: i32 = 1;
const STDERR: i32 = 2;

#[link(name="c")]
unsafe extern "C" {
    fn write(filedes: i32, buf: *const core::ffi::c_void, nbyte: usize);
}

pub fn write_to(filedes: i32, s: &str) {
    unsafe {
        write(filedes, s.as_ptr() as *const _, s.len());
    }
}

pub struct StdOut;
pub struct StdErr;

impl core::fmt::Write for StdOut {
    fn write_str(&mut self, s: &str) -> Result<(), core::fmt::Error> {
        write_to(STDOUT, s);
        Ok(())
    }
}

impl core::fmt::Write for StdErr {
    fn write_str(&mut self, s: &str) -> Result<(), core::fmt::Error> {
        write_to(STDERR, s);
        Ok(())
    }
}

#[allow(unused)]
pub fn write_stdout(args: core::fmt::Arguments) {
    use core::fmt::Write;
    StdOut.write_fmt(args).unwrap();
}

#[allow(unused)]
pub fn write_stderr(args: core::fmt::Arguments) {
    use core::fmt::Write;
    StdErr.write_fmt(args).unwrap();
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => {{
        $crate::print_no_std::write_stdout(format_args!($($arg)*));
    }}
}

#[macro_export]
macro_rules! println {
    () => {{
        $crate::print!("\n");
    }};

    ($($arg:tt)*) => {{
        $crate::print!("{}\n", format_args!($($arg)*));
    }}
}

#[macro_export]
macro_rules! eprint {
    ($($arg:tt)*) => {{
        $crate::print_no_std::write_stderr(format_args!($($arg)*));
    }}
}

#[macro_export]
macro_rules! eprintln {
    () => {{
        eprint!("\n");
    }};

    ($($arg:tt)*) => {{
        eprint!("{}\n", format_args!($($arg)*));
    }}
}

pub use crate::print;
pub use crate::println;
pub use crate::eprint;
pub use crate::eprintln;
