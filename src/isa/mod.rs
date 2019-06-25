use std::fmt::{Display, Formatter, Result as FmtResult};

#[macro_use] pub mod encoding;

pub mod w65816;

/// Represents a full memory offset for the SNES.
#[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug, Hash)]
pub struct SnesOffset(pub u8, pub u16);

impl Display for SnesOffset {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "${:02x}:{:04x}", self.0, self.1)
    }
}

/// An error that occurred during instruction encoding or decoding.
#[derive(Debug)]
pub enum IsaError {
    /// Reached the end of the data.
    Eof,
    /// An invalid instruction was found.
    InvalidInstruction,
}

/// A wrapper for instruction related operations that may fail.
pub type IsaResult<T> = Result<T, IsaError>;
