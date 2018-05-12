#[derive(Clone, Copy, Debug)]
pub enum OpCode {
    Return = 0x01,
    Constant = 0x02,
    Neg = 0x03,
    Add = 0x04,
    Subtract = 0x05,
    Multiply = 0x06,
    Divide = 0x07,
}

pub trait TryFrom<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Error;

    /// Performs the conversion.
    fn try_from(T) -> Result<Self, Self::Error>;
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
    }
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(original: u8) -> Result<Self, Self::Error> {
        match original {
            1 => Ok(OpCode::Return),
            2 => Ok(OpCode::Constant),
            3 => Ok(OpCode::Neg),
            4 => Ok(OpCode::Add),
            5 => Ok(OpCode::Subtract),
            6 => Ok(OpCode::Multiply),
            7 => Ok(OpCode::Divide),
            _ => Err(()),
        }
    }
}
