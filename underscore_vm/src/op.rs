macro_rules! op {
    ($($body:tt)*) =>   {
        op_inner! {
            #[derive(Clone, Copy, Debug)]
            pub enum OpCode {
                $($body)*
            }
        }
    }
}

macro_rules! op_inner {
    ($i:item) => {
        $i
    };
}

op! {
    Return,
    Constant8,
    Constant32,
    Constant64,
    Neg,
    Add,
    Subtract ,
    Multiply ,
    Divide,
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
        use self::OpCode::*;
        match original {
            0 => Ok(Return),
            1 => Ok(Constant8),
            2 => Ok(Constant32),
            3 => Ok(Constant64),
            4 => Ok(Neg),
            5 => Ok(Add),
            6 => Ok(Subtract),
            7 => Ok(Multiply),
            8 => Ok(Divide),
            _ => Err(()),
        }
    }
}
