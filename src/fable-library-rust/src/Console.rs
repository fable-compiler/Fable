#[cfg(not(feature = "no_std"))]
pub mod Console_ {
    use crate::Native_::{Any, LrcPtr, box_};
    use crate::String_::string;

    #[derive(Clone, Copy)]
    enum Writer {
        Out,
        Error,
    }

    pub fn getOut() -> LrcPtr<dyn Any> {
        box_(Writer::Out)
    }

    pub fn getError() -> LrcPtr<dyn Any> {
        box_(Writer::Error)
    }

    pub fn writeLine(writer: LrcPtr<dyn Any>, message: string) {
        match writer.downcast_ref::<Writer>() {
            Some(Writer::Out) => println!("{}", message.as_str()),
            Some(Writer::Error) => eprintln!("{}", message.as_str()),
            None => panic!("Unsupported TextWriter"),
        }
    }

    pub fn writeLineEmpty(writer: LrcPtr<dyn Any>) {
        writeLine(writer, string(""));
    }
}
