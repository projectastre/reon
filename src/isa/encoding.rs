use crate::isa::{SnesOffset, IsaResult, SnesImmediate, Rel8, Rel16, IsaError};

/// A struct representing the encoding state.
pub struct EncodeCursor<'a> {
    offset: usize,
    data: &'a mut Vec<u8>,
}
impl <'a> EncodeCursor<'a> {
    pub fn new(data: &'a mut Vec<u8>) -> Self {
        EncodeCursor { offset: 0, data }
    }

    pub fn push_byte(&mut self, byte: u8) {
        if self.offset == self.data.len() {
            self.data.push(byte);
        } else {
            self.data[self.offset] = byte;
        }
        self.offset += 1;
    }
}

/// A trait representing the encoding of an instruction to machine code.
pub trait Encode {
    /// Encode an instruction to machine code.
    fn encode(&self, w: &mut EncodeCursor<'_>) -> IsaResult<()>;
}

impl Encode for u8 {
    fn encode(&self, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        w.push_byte(*self);
        Ok(())
    }
}
impl Encode for u16 {
    fn encode(&self, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        w.push_byte(*self as u8);
        w.push_byte((*self >> 8) as u8);
        Ok(())
    }
}
impl Encode for SnesOffset {
    fn encode(&self, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        self.1.encode(w)?;
        self.0.encode(w)?;
        Ok(())
    }
}
impl Encode for SnesImmediate {
    fn encode(&self, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        match *self {
            SnesImmediate::Imm8(v) => v.encode(w)?,
            SnesImmediate::Imm16(v) => v.encode(w)?,
        }
        Ok(())
    }
}
impl Encode for Rel8 {
    fn encode(&self, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        self.0.encode(w)?;
        Ok(())
    }
}
impl Encode for Rel16 {
    fn encode(&self, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        self.0.encode(w)?;
        Ok(())
    }
}

pub fn encode_op(v: &impl OpcodeClass, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
    v.encode_operands(w)
}
pub fn encode(v: &impl Encode, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
    v.encode(w)
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

    pub fn pull_byte(&mut self) -> IsaResult<u8> {
        if self.offset == self.data.len() {
            Err(IsaError::Eof)
        } else {
            let v = self.data[self.offset];
            self.offset += 1;
            Ok(v)
        }
    }
}

/// A struct representing the context an instruction is being decoded in.
pub struct DecodeContext {
    pub emulation_mode: bool,
    pub a_8_bit: bool,
    pub xy_8_bit: bool,
}

/// A trait representing the decoding of an instruction from machine code.
pub trait Decode: Sized {
    /// Decode an instruction from machine code.
    fn decode(r: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self>;
}

impl Decode for u8 {
    fn decode(r: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self> {
        Ok(r.pull_byte()?)
    }
}
impl Decode for u16 {
    fn decode(r: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self> {
        Ok((r.pull_byte()? as u16) | ((r.pull_byte()? as u16) << 8))
    }
}
impl Decode for SnesOffset {
    fn decode(r: &mut DecodeCursor<'_>, ctx: &DecodeContext) -> IsaResult<Self> {
        let offset = u16::decode(r, ctx)?;
        let bank = u8::decode(r, ctx)?;
        Ok(SnesOffset(bank, offset))
    }
}
impl Decode for Rel8 {
    fn decode(r: &mut DecodeCursor<'_>, ctx: &DecodeContext) -> IsaResult<Self> {
        Ok(Rel8(u8::decode(r, ctx)?))
    }
}
impl Decode for Rel16 {
    fn decode(r: &mut DecodeCursor<'_>, ctx: &DecodeContext) -> IsaResult<Self> {
        Ok(Rel16(u16::decode(r, ctx)?))
    }
}

fn decode_imm(
    r: &mut DecodeCursor<'_>, ctx: &DecodeContext, is_8_bit: bool,
) -> IsaResult<SnesImmediate> {
    if is_8_bit {
        Ok(SnesImmediate::Imm8(u8::decode(r, ctx)?))
    } else {
        Ok(SnesImmediate::Imm16(u16::decode(r, ctx)?))
    }
}

pub fn decode_a<T>(
    _: u8, _: u8, r: &mut DecodeCursor<'_>, ctx: &DecodeContext,
) -> IsaResult<SnesImmediate> {
    decode_imm(r, ctx, ctx.emulation_mode || ctx.a_8_bit)
}
pub fn decode_xy<T>(
    _: u8, _: u8, r: &mut DecodeCursor<'_>, ctx: &DecodeContext,
) -> IsaResult<SnesImmediate> {
    decode_imm(r, ctx, ctx.emulation_mode || ctx.xy_8_bit)
}

pub fn decode_op<T: OpcodeClass>(
    base: u8, op: u8, r: &mut DecodeCursor<'_>, ctx: &DecodeContext,
) -> IsaResult<T> {
    T::decode_operands(op.wrapping_sub(base), r, ctx)
}
pub fn decode<T: Decode>(
    _: u8, _: u8, r: &mut DecodeCursor<'_>, ctx: &DecodeContext,
) -> IsaResult<T> {
    T::decode(r, ctx)
}

/// A trait representing the length of an instruction.
pub trait InstructionLength {
    fn instruction_len(&self) -> u8;
}

impl InstructionLength for u8 {
    fn instruction_len(&self) -> u8 {
        1
    }
}
impl InstructionLength for u16 {
    fn instruction_len(&self) -> u8 {
        2
    }
}
impl InstructionLength for SnesOffset {
    fn instruction_len(&self) -> u8 {
        3
    }
}
impl InstructionLength for SnesImmediate {
    fn instruction_len(&self) -> u8 {
        match *self {
            SnesImmediate::Imm8(_) => 1,
            SnesImmediate::Imm16(_) => 2,
        }
    }
}
impl InstructionLength for Rel8 {
    fn instruction_len(&self) -> u8 {
        1
    }
}
impl InstructionLength for Rel16 {
    fn instruction_len(&self) -> u8 {
        2
    }
}

/// A trait representing an operand that affects the opcode itself.
pub trait OpcodeClass: Sized {
    fn accepts_op(base: u8, op: u8) -> bool;
    fn op(&self, base: u8) -> u8;

    fn encode_operands(&self, _: &mut EncodeCursor<'_>) -> IsaResult<()>;
    fn decode_operands(_: u8, _: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self>;
}

/// A helper macro for simple ISAs with an opcode followed by operands.
macro_rules! isa_instruction_table {
    (@parse_inner_common
        [$s1:ident $s2:ident $s3:ident $enum_name:ident ($vis:vis) $metas:tt
         [$($enum_contents:tt)*]
         $op_impl:tt
         $is_op_before:tt
         $is_op_impl:tt
         [$($encode_impl:tt)*]
         $decode_before:tt
         $decode_impl:tt
         [$($len_impl:tt)*]]
        [$($rest:tt)*]
        $opcode:literal $instr:ident $(($($enc:ident $sub_len:literal $name:ident: $ty:ty,)*))?
    ) => {
        isa_instruction_table!(@parse
            [
                $s1 $s2 $s3 $enum_name ($vis) $metas
                [$instr $(($($ty,)*))?, $($enum_contents)*]
                $op_impl
                $is_op_before
                $is_op_impl
                [$enum_name::$instr $(($($name,)*))? => {
                    $($(
                        crate::isa::encoding::$enc(&$name, $s1)?;
                    )*)?
                }, $($encode_impl)*]
                $decode_before
                $decode_impl
                [$enum_name::$instr $(($($name,)*))? => {
                    1 $($(
                        + (crate::isa::encoding::InstructionLength::instruction_len(&$name)
                           - $sub_len)
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
         $is_op_before:tt
         [$($is_op_impl:tt)*]
         $encode_impl:tt
         $decode_before:tt
         [$($decode_impl:tt)*]
         $len_impl:tt]
        [$($rest:tt)*]
        $opcode:literal $instr:ident
        $(($($enc:ident $dec:ident $sub_len:literal $name:ident: $ty:ty),*))?
    ) => {
        isa_instruction_table!(@parse_inner_common
            [
                $s1 $s2 $s3 $enum_name ($vis) $metas
                $enum_contents
                [$enum_name::$instr $(($($name,)*))? => $opcode, $($op_impl)*]
                $is_op_before
                [$opcode => true, $($is_op_impl)*]
                $encode_impl
                $decode_before
                [$opcode => {
                    $enum_name::$instr $(($(
                        crate::isa::encoding::$dec::<$ty>($opcode, $s3, $s1, $s2)?,
                    )*))?
                }, $($decode_impl)*]
                $len_impl
            ]
            [$($rest)*]
            $opcode $instr $(($($enc $sub_len $name: $ty,)*))?
        );
    };
    (@parse_inner_class
        [$s1:ident $s2:ident $s3:ident $enum_name:ident ($vis:vis) $metas:tt
         $enum_contents:tt
         [$($op_impl:tt)*]
         [$($is_op_before:tt)*]
         $is_op_impl:tt
         $encode_impl:tt
         [$($decode_before:tt)*]
         $decode_impl:tt
         $len_impl:tt]
        [$($rest:tt)*]
        $opcode:literal $instr:ident ($class_name:ident: $class_ty:ty)
        $(($($enc:ident $dec:ident $sub_len:literal $name:ident: $ty:ty),*))?
    ) => {
        isa_instruction_table!(@parse_inner_common
            [
                $s1 $s2 $s3 $enum_name ($vis) $metas
                $enum_contents
                [$enum_name::$instr $(($($name,)*))? => {
                    crate::isa::encoding::OpcodeClass::op(&$class_name, $opcode)
                }, $($op_impl)*]
                [ {
                    let accepts =
                        <$class_ty as crate::isa::encoding::OpcodeClass>::accepts_op($opcode, $s3);
                    if accepts { return true }
                } $($is_op_before)* ]
                $is_op_impl
                $encode_impl
                [ {
                    let accepts =
                        <$class_ty as crate::isa::encoding::OpcodeClass>::accepts_op($opcode, $s3);
                    if accepts {
                        return Ok($enum_name::$instr $(($(
                            crate::isa::encoding::$dec::<$ty>($opcode, $s3, $s1, $s2)?,
                        )*))?);
                    }
                } $($decode_before)* ]
                $decode_impl
                $len_impl
            ]
            [$($rest)*]
            $opcode $instr $(($($enc $sub_len $name: $ty,)*))?
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident, $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner $state [$($rest)*] $opcode $instr);
    };
    (@parse $state:tt [$opcode:literal $instr:ident (#imm_a), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner $state [$($rest)*]
            $opcode $instr (encode decode_a 0 a: crate::isa::SnesImmediate)
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident (#imm_xy), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner $state [$($rest)*]
            $opcode $instr (encode decode_xy 0 a: crate::isa::SnesImmediate)
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident($a:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner $state [$($rest)*]
            $opcode $instr (encode decode 0 a: $a)
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident($a:ty, $b:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner $state [$($rest)*]
            $opcode $instr (encode decode 0 a: $a, encode decode 0 b: $b)
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident(class $a:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner_class $state [$($rest)*]
            $opcode $instr (a: $a) (encode_op decode_op 1 a: $a)
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident(class $a:ty, $b:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner_class $state [$($rest)*]
            $opcode $instr (a: $a) (encode_op decode_op 1 a: $a, encode decode 0 b: $b)
        );
    };
    (@parse
        [$s1:ident $s2:ident $s3:ident $name:ident ($vis:vis) ($(#[$meta:meta])*)
         [$($enum_contents:tt)*]
         [$($op_impl:tt)*]
         [$($is_op_before:tt)*]
         [$($is_op_impl:tt)*]
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
        impl $name {
            #[allow(unused_variables)]
            pub fn op(&self) -> u8 {
                match *self { $($op_impl)* }
            }
            #[allow(unused_variables)]
            pub fn is_op($s3: u8) -> bool {
                $($is_op_before)*
                match $s3 {
                    $($is_op_impl)*
                    _ => false,
                }
            }
        }
        impl crate::isa::encoding::Encode for $name {
            #[allow(unreachable_code, unused_variables, unused_imports)]
            fn encode(
                &self, w: &mut crate::isa::encoding::EncodeCursor<'_>,
            ) -> crate::isa::IsaResult<()> {
                w.push_byte(self.op());
                self.encode_operands(w)
            }
        }
        impl crate::isa::encoding::Decode for $name {
            #[allow(unreachable_patterns, unreachable_code, unused_variables, unused_imports)]
            fn decode(
                r: &mut crate::isa::encoding::DecodeCursor<'_>,
                ctx: &crate::isa::encoding::DecodeContext,
            ) -> crate::isa::IsaResult<Self> {
                let op = r.pull_byte()?;
                Self::decode_operands(op, r, ctx)
            }
        }
        impl crate::isa::encoding::InstructionLength for $name {
            #[allow(unreachable_code, unused_variables, unused_imports)]
            fn instruction_len(&self) -> u8 {
                match *self { $($len_impl)* }
            }
        }
        impl crate::isa::encoding::OpcodeClass for $name {
            fn accepts_op(base: u8, op: u8) -> bool {
                Self::is_op(op.wrapping_sub(base))
            }
            fn op(&self, base: u8) -> u8 {
                return base.wrapping_add(self.op());
            }

            #[allow(unreachable_code, unused_variables, unused_imports)]
            fn encode_operands(
                &self, $s1: &mut crate::isa::encoding::EncodeCursor<'_>,
            ) -> crate::isa::IsaResult<()> {
                match *self { $($encode_impl)* }
                Ok(())
            }
            #[allow(unreachable_patterns, unreachable_code, unused_variables, unused_imports)]
            fn decode_operands(
                $s3: u8,
                $s1: &mut crate::isa::encoding::DecodeCursor<'_>,
                $s2: &crate::isa::encoding::DecodeContext,
            ) -> crate::isa::IsaResult<Self> {
                $($decode_before)*
                Ok(match $s3 {
                    $($decode_impl)*
                    _ => return Err(crate::isa::IsaError::InvalidInstruction),
                })
            }
        }
    };
    ($($(#[$meta:meta])* $vis:vis table $name:ident { $($body:tt)* })*) => {$(
        isa_instruction_table!(@parse
            [s1 s2 s3 $name ($vis) ($(#[$meta])*) [] [] [] [] [] [] [] []]
            [$($body)*]
        );
    )*};
}