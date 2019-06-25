use crate::isa::{IsaResult, IsaError, SnesOffset};

/// A struct representing the encoding state.
pub struct EncodeCursor<'a> {
    offset: usize,
    data: &'a mut Vec<u8>,
}
impl <'a> EncodeCursor<'a> {
    pub fn new(data: &'a mut Vec<u8>) -> Self {
        EncodeCursor { offset: 0, data }
    }

    pub fn seek(&mut self, offset: usize) {
        if offset > self.data.len() {
            panic!("Offset out of bounds.")
        }
        self.offset = offset;
    }
    pub fn len(&self) -> usize {
        self.data.len()
    }
    pub fn offset(&self) -> usize {
        self.offset
    }
    pub fn push_byte(&mut self, byte: u8) {
        if self.offset >= self.data.len() {
            self.data.push(byte);
        } else {
            self.data[self.offset] = byte;
        }
        self.offset += 1;
    }
}

/// A struct representing the decoding state.
pub struct DecodeCursor<'a> {
    offset: usize,
    data: &'a [u8],
}
impl <'a> DecodeCursor<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        DecodeCursor { offset: 0, data }
    }

    pub fn seek(&mut self, offset: usize) {
        if offset >= self.data.len() {
            panic!("Offset out of bounds.")
        }
        self.offset = offset;
    }
    pub fn len(&self) -> usize {
        self.data.len()
    }
    pub fn offset(&self) -> usize {
        self.offset
    }
    pub fn pull_byte(&mut self) -> IsaResult<u8> {
        if let Some(v) = self.data.get(self.offset) {
            self.offset += 1;
            Ok(*v)
        } else {
            Err(IsaError::Eof)
        }
    }
    pub fn pull_bytes(&mut self, count: usize) -> IsaResult<&[u8]> {
        if let Some(slice) = self.data.get(self.offset..self.offset + count) {
            self.offset += count;
            Ok(slice)
        } else {
            Err(IsaError::Eof)
        }
    }
}

/// A struct representing the context an instruction is being decoded in.
pub struct DecodeContext {
    /// State of the 6502 emulation flag.
    pub emulation_mode: bool,
    /// State of the M flag
    pub a_8_bit: bool,
    /// State of the X flag
    pub xy_8_bit: bool,
}

/// A trait representing the encoding of an instruction to machine code.
pub trait Encoder {
    /// The type this encoder encodes to.
    type Target: Copy;
    /// Encode an instruction to machine code.
    fn encode(v: Self::Target, w: &mut EncodeCursor<'_>) -> IsaResult<()>;
    /// Decode an instruction from machine code.
    fn decode(r: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self::Target>;
    /// The length of this instruction.
    fn instruction_len(v: Self::Target) -> u8;
}

impl Encoder for u8 {
    type Target = Self;
    fn encode(v: Self::Target, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        w.push_byte(v);
        Ok(())
    }
    fn decode(r: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self::Target> {
        Ok(r.pull_byte()?)
    }
    fn instruction_len(_: Self::Target) -> u8 {
        1
    }
}
impl Encoder for u16 {
    type Target = Self;
    fn encode(v: Self::Target, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        w.push_byte(v as u8);
        w.push_byte((v >> 8) as u8);
        Ok(())
    }
    fn decode(r: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self::Target> {
        let bytes = r.pull_bytes(2)?;
        Ok(bytes[0] as u16 | ((bytes[1] as u16) << 8))
    }
    fn instruction_len(_: Self::Target) -> u8 {
        2
    }
}
impl Encoder for SnesOffset {
    type Target = Self;
    fn encode(v: Self::Target, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        u16::encode(v.1, w)?;
        u8::encode(v.0, w)?;
        Ok(())
    }
    fn decode(r: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self::Target> {
        let bytes = r.pull_bytes(3)?;
        let offset = bytes[0] as u16 | ((bytes[1] as u16) << 8);
        Ok(SnesOffset(bytes[2], offset))
    }
    fn instruction_len(_: Self::Target) -> u8 {
        3
    }
}

/// A trait representing a table of opcodes.
pub trait OpcodeTable: Sized {
    fn op(self, base: u8) -> u8;
    fn encode_operands(self, _: &mut EncodeCursor<'_>) -> IsaResult<()>;
    fn decode_operands(
        _: u8, _: &mut DecodeCursor<'_>, _: &DecodeContext,
    ) -> IsaResult<Option<Self>>;
}

/// A trait representing a decodable instruction.
pub trait InstructionType: Sized + Copy + Encoder<Target = Self> {
    fn encode(self, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        <Self as Encoder>::encode(self, w)
    }
    fn decode(w: &mut DecodeCursor<'_>, ctx: &DecodeContext) -> IsaResult<Self> {
        <Self as Encoder>::decode(w, ctx)
    }
    fn instruction_len(self) -> u8 {
        <Self as Encoder>::instruction_len(self)
    }
}

/// A helper macro for simple ISAs with an opcode followed by operands.
macro_rules! isa_instruction_table {
    (@encode $ty:ty, $val:expr, $r:expr) => {
        <$ty as crate::isa::encoding::Encoder>::encode($val, $r)
    };
    (@encode_op $ty:ty, $val:expr, $r:expr) => {
        crate::isa::encoding::OpcodeTable::encode_operands($val, $r)
    };
    (@parse_inner_common
        [$s1:ident $s2:ident $s3:ident $enum_name:ident ($vis:vis) $metas:tt
         [$($enum_contents:tt)*]
         $op_impl:tt
         [$($encode_impl:tt)*]
         $decode_before:tt
         $decode_impl:tt
         [$($len_impl:tt)*]]
        [$($rest:tt)*]
        $opcode:literal $instr:ident $add_len:literal $(($($enc:ident $name:ident: $ty:ty,)*))?
    ) => {
        isa_instruction_table!(@parse
            [
                $s1 $s2 $s3 $enum_name ($vis) $metas
                [
                    $instr $(($(<$ty as crate::isa::encoding::Encoder>::Target,)*))?,
                    $($enum_contents)*
                ]
                $op_impl
                [$enum_name::$instr $(($($name,)*))? => {
                    $($(
                        isa_instruction_table!(@$enc $ty, $name, $s1)?;
                    )*)?
                }, $($encode_impl)*]
                $decode_before
                $decode_impl
                [$enum_name::$instr $(($($name,)*))? => {
                    $add_len $($(
                        + (<$ty as crate::isa::encoding::Encoder>::instruction_len($name))
                    )*)?
                }, $($len_impl)*]
            ]
            [$($rest)*]
        );
    };
    (@parse_inner
        [$s1:ident $s2:ident $s3:ident $enum_name:ident ($vis:vis) $metas:tt
         $enum_contents:tt
         [$($op_impl:tt)*]
         $encode_impl:tt
         $decode_before:tt
         [$($decode_impl:tt)*]
         $len_impl:tt]
        [$($rest:tt)*]
        $opcode:literal $instr:ident
        $(($($name:ident: $ty:ty),*))?
    ) => {
        isa_instruction_table!(@parse_inner_common
            [
                $s1 $s2 $s3 $enum_name ($vis) $metas
                $enum_contents
                [$enum_name::$instr $(($($name,)*))? => $opcode, $($op_impl)*]
                $encode_impl
                $decode_before
                [$opcode => {
                    return Ok(Some($enum_name::$instr $(($(
                        <$ty as crate::isa::encoding::Encoder>::decode($s1, $s2)?,
                    )*))?));
                }, $($decode_impl)*]
                $len_impl
            ]
            [$($rest)*]
            $opcode $instr 1 $(($(encode $name: $ty,)*))?
        );
    };
    (@parse_inner_class
        [$s1:ident $s2:ident $s3:ident $enum_name:ident ($vis:vis) $metas:tt
         $enum_contents:tt
         [$($op_impl:tt)*]
         $encode_impl:tt
         [$($decode_before:tt)*]
         $decode_impl:tt
         $len_impl:tt]
        [$($rest:tt)*]
        $opcode:literal $instr:ident ($class_name:ident: $class_ty:ty)
        ($($name:ident: $ty:ty),*)
    ) => {
        isa_instruction_table!(@parse_inner_common
            [
                $s1 $s2 $s3 $enum_name ($vis) $metas
                $enum_contents
                [$enum_name::$instr ($class_name, $($name,)*) => {
                    crate::isa::encoding::OpcodeTable::op($class_name, $opcode)
                }, $($op_impl)*]
                $encode_impl
                [ if let Some(class) =
                    <$class_ty as crate::isa::encoding::OpcodeTable>::decode_operands(
                        $s3.wrapping_sub($opcode), $s1, $s2,
                    )?
                {
                    return Ok(Some($enum_name::$instr (class, $(
                        <$ty as crate::isa::encoding::Encoder>::decode($s1, $s2)?,
                    )*)));
                } $($decode_before)* ]
                $decode_impl
                $len_impl
            ]
            [$($rest)*]
            $opcode $instr 0 (encode_op $class_name: $class_ty, $(encode $name: $ty,)*)
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident, $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner $state [$($rest)*] $opcode $instr);
    };
    (@parse $state:tt [$opcode:literal $instr:ident($a:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner $state [$($rest)*]
            $opcode $instr (_a: $a)
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident($a:ty, $b:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner $state [$($rest)*]
            $opcode $instr (_a: $a, _b: $b)
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident(class $a:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner_class $state [$($rest)*]
            $opcode $instr (_a: $a) ()
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident(class $a:ty, $b:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner_class $state [$($rest)*]
            $opcode $instr (_a: $a) (_b: $b)
        );
    };
    (@parse
        [$s1:ident $s2:ident $s3:ident $name:ident ($vis:vis) ($(#[$meta:meta])*)
         [$($enum_contents:tt)*]
         [$($op_impl:tt)*]
         [$($encode_impl:tt)*]
         [$($decode_before:tt)*]
         [$($decode_impl:tt)*]
         [$($len_impl:tt)*]]
        []
    ) => {
        $(#[$meta])*
        #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug, Hash)]
        $vis enum $name {
            $($enum_contents)*
        }
        impl crate::isa::encoding::Encoder for $name {
            type Target = $name;
            fn encode(
                v: $name, w: &mut crate::isa::encoding::EncodeCursor<'_>,
            ) -> crate::isa::IsaResult<()> {
                w.push_byte(crate::isa::encoding::OpcodeTable::op(v, 0));
                crate::isa::encoding::OpcodeTable::encode_operands(v, w)
            }
            fn decode(
                r: &mut crate::isa::encoding::DecodeCursor<'_>,
                ctx: &crate::isa::encoding::DecodeContext,
            ) -> crate::isa::IsaResult<$name> {
                let op = r.pull_byte()?;
                if let Some(instr) =
                    <$name as crate::isa::encoding::OpcodeTable>::decode_operands(op, r, ctx)?
                {
                    Ok(instr)
                } else {
                    Err(crate::isa::IsaError::InvalidInstruction)
                }
            }
            fn instruction_len(v: $name) -> u8 {
                match v { $($len_impl)* }
            }
        }
        impl crate::isa::encoding::OpcodeTable for $name {
            fn op(self, base: u8) -> u8 {
                return base.wrapping_add(match self { $($op_impl)* });
            }
            fn encode_operands(
                self, $s1: &mut crate::isa::encoding::EncodeCursor<'_>,
            ) -> crate::isa::IsaResult<()> {
                match self { $($encode_impl)* }
                Ok(())
            }
            fn decode_operands(
                $s3: u8,
                $s1: &mut crate::isa::encoding::DecodeCursor<'_>,
                $s2: &crate::isa::encoding::DecodeContext,
            ) -> crate::isa::IsaResult<Option<Self>> {
                $($decode_before)*
                match $s3 {
                    $($decode_impl)*
                    _ => { }
                }
                Ok(None)
            }
        }
    };
    ($($(#[$meta:meta])* $vis:vis table $name:ident { $($body:tt)* })*) => {$(
        isa_instruction_table!(@parse
            [s1 s2 s3 $name ($vis) ($(#[$meta])*) [] [] [] [] [] []]
            [$($body)*]
        );
    )*};
}