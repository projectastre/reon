use byteorder::*;
use std::io::{Read, Write};

use crate::isa::{SnesOffset, IsaResult, SnesImmediate, Rel8, Rel16};

/// A trait representing the encoding of an instruction to machine code.
pub trait Encode {
    /// Encode an instruction to machine code.
    fn encode(&self, w: &mut impl Write) -> IsaResult<()>;
}

impl Encode for u8 {
    #[inline(always)]
    fn encode(&self, w: &mut impl Write) -> IsaResult<()> {
        w.write_u8(*self)?;
        Ok(())
    }
}
impl Encode for u16 {
    #[inline(always)]
    fn encode(&self, w: &mut impl Write) -> IsaResult<()> {
        w.write_u16::<LE>(*self)?;
        Ok(())
    }
}
impl Encode for SnesOffset {
    #[inline(always)]
    fn encode(&self, w: &mut impl Write) -> IsaResult<()> {
        self.1.encode(w)?;
        self.0.encode(w)?;
        Ok(())
    }
}
impl Encode for SnesImmediate {
    #[inline(always)]
    fn encode(&self, w: &mut impl Write) -> IsaResult<()> {
        match *self {
            SnesImmediate::Imm8(v) => w.write_u8(v)?,
            SnesImmediate::Imm16(v) => w.write_u16::<LE>(v)?,
        }
        Ok(())
    }
}
impl Encode for Rel8 {
    #[inline(always)]
    fn encode(&self, w: &mut impl Write) -> IsaResult<()> {
        w.write_u8(self.0)?;
        Ok(())
    }
}
impl Encode for Rel16 {
    #[inline(always)]
    fn encode(&self, w: &mut impl Write) -> IsaResult<()> {
        w.write_u16::<LE>(self.0)?;
        Ok(())
    }
}

#[inline(always)]
pub fn encode_op(v: &impl OpcodeClass, w: &mut impl Write) -> IsaResult<()> {
    v.encode_operands(w)
}
#[inline(always)]
pub fn encode(v: &impl Encode, w: &mut impl Write) -> IsaResult<()> {
    v.encode(w)
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
    fn decode(r: &mut impl Read, _: &DecodeContext) -> IsaResult<Self>;
}

impl Decode for u8 {
    #[inline(always)]
    fn decode(r: &mut impl Read, _: &DecodeContext) -> IsaResult<Self> {
        Ok(r.read_u8()?)
    }
}
impl Decode for u16 {
    #[inline(always)]
    fn decode(r: &mut impl Read, _: &DecodeContext) -> IsaResult<Self> {
        Ok(r.read_u16::<LE>()?)
    }
}
impl Decode for SnesOffset {
    #[inline(always)]
    fn decode(r: &mut impl Read, ctx: &DecodeContext) -> IsaResult<Self> {
        let offset = u16::decode(r, ctx)?;
        let bank = u8::decode(r, ctx)?;
        Ok(SnesOffset(bank, offset))
    }
}
impl Decode for Rel8 {
    #[inline(always)]
    fn decode(r: &mut impl Read, _: &DecodeContext) -> IsaResult<Self> {
        Ok(Rel8(r.read_u8()?))
    }
}
impl Decode for Rel16 {
    #[inline(always)]
    fn decode(r: &mut impl Read, _: &DecodeContext) -> IsaResult<Self> {
        Ok(Rel16(r.read_u16::<LE>()?))
    }
}

fn decode_imm(r: &mut impl Read, is_8_bit: bool) -> IsaResult<SnesImmediate> {
    if is_8_bit {
        Ok(SnesImmediate::Imm8(r.read_u8()?))
    } else {
        Ok(SnesImmediate::Imm16(r.read_u16::<LE>()?))
    }
}

#[inline(always)]
pub fn decode_a<T, R: Read>(
    _: u8, _: u8, r: &mut R, ctx: &DecodeContext,
) -> IsaResult<SnesImmediate> {
    decode_imm(r, ctx.emulation_mode || ctx.a_8_bit)
}
#[inline(always)]
pub fn decode_xy<T, R: Read>(
    _: u8, _: u8, r: &mut R, ctx: &DecodeContext,
) -> IsaResult<SnesImmediate> {
    decode_imm(r, ctx.emulation_mode || ctx.xy_8_bit)
}

#[inline(always)]
pub fn decode_op<T: OpcodeClass, R: Read>(
    base: u8, op: u8, r: &mut R, ctx: &DecodeContext,
) -> IsaResult<T> {
    T::decode_operands(op.wrapping_sub(base), r, ctx)
}
#[inline(always)]
pub fn decode<T: Decode, R: Read>(
    _: u8, _: u8, r: &mut R, ctx: &DecodeContext,
) -> IsaResult<T> {
    T::decode(r, ctx)
}

/// A trait representing the length of an instruction.
pub trait InstructionLength {
    fn instruction_len(&self) -> u8;
}

impl InstructionLength for u8 {
    #[inline(always)]
    fn instruction_len(&self) -> u8 {
        1
    }
}
impl InstructionLength for u16 {
    #[inline(always)]
    fn instruction_len(&self) -> u8 {
        2
    }
}
impl InstructionLength for SnesOffset {
    #[inline(always)]
    fn instruction_len(&self) -> u8 {
        3
    }
}
impl InstructionLength for SnesImmediate {
    #[inline(always)]
    fn instruction_len(&self) -> u8 {
        match *self {
            SnesImmediate::Imm8(_) => 1,
            SnesImmediate::Imm16(_) => 2,
        }
    }
}
impl InstructionLength for Rel8 {
    #[inline(always)]
    fn instruction_len(&self) -> u8 {
        1
    }
}
impl InstructionLength for Rel16 {
    #[inline(always)]
    fn instruction_len(&self) -> u8 {
        2
    }
}

/// A trait representing an operand that affects the opcode itself.
pub trait OpcodeClass: Sized {
    fn accepts_op(base: u8, op: u8) -> bool;
    fn op(&self, base: u8) -> u8;

    fn decode_operands(_: u8, _: &mut impl Read, _: &DecodeContext) -> IsaResult<Self>;
    fn encode_operands(&self, _: &mut impl Write) -> IsaResult<()>;
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
                        crate::isa::encoding::$dec::<$ty, _>($opcode, $s3, $s1, $s2)?,
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
                            crate::isa::encoding::$dec::<$ty, _>($opcode, $s3, $s1, $s2)?,
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
            fn encode(&self, w: &mut impl ::std::io::Write) -> crate::isa::IsaResult<()> {
                use byteorder::*;
                w.write_u8(self.op())?;
                self.encode_operands(w)
            }
        }
        impl crate::isa::encoding::Decode for $name {
            #[allow(unreachable_patterns, unreachable_code, unused_variables, unused_imports)]
            fn decode(
                r: &mut impl ::std::io::Read, ctx: &crate::isa::encoding::DecodeContext,
            ) -> crate::isa::IsaResult<Self> {
                use byteorder::*;
                let op = r.read_u8()?;
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

            #[allow(unreachable_patterns, unreachable_code, unused_variables, unused_imports)]
            fn decode_operands(
                $s3: u8, $s1: &mut impl ::std::io::Read, $s2: &crate::isa::encoding::DecodeContext,
            ) -> crate::isa::IsaResult<Self> {
                use byteorder::*;
                $($decode_before)*
                Ok(match $s3 {
                    $($decode_impl)*
                    _ => return Err(crate::isa::IsaError::InvalidInstruction),
                })
            }
            #[allow(unreachable_code, unused_variables, unused_imports)]
            fn encode_operands(
                &self, $s1: &mut impl ::std::io::Write,
            ) -> crate::isa::IsaResult<()> {
                use byteorder::*;
                match *self { $($encode_impl)* }
                Ok(())
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