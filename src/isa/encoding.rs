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
}

/// A trait representing a decodable instruction.
pub trait InstructionType: Sized + Copy + Encoder<Target = Self> {
    /// Encode an instruction to machine code.
    fn encode(self, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
        <Self as Encoder>::encode(self, w)
    }
    /// Decode an instruction from machine code.
    fn decode(w: &mut DecodeCursor<'_>, ctx: &DecodeContext) -> IsaResult<Self> {
        <Self as Encoder>::decode(w, ctx)
    }
    /// The length of this instruction.
    fn instruction_len(self) -> u8 {
        <Self as Encoder>::instruction_len(self)
    }
}

/// The function type used in decode tables.
pub(crate) type DecodeFn<T> = fn(&mut DecodeCursor<'_>, &DecodeContext) -> IsaResult<T>;

/// The array type used for decode tables.
pub(crate) type DecodeTable<T> = [DecodeFn<T>; 256];

/// The trait used to map decode tables.
pub(crate) trait DecodeMap<A> {
    type Into;
    fn apply(a: A, r: &mut DecodeCursor<'_>, ctx: &DecodeContext) -> IsaResult<Self::Into>;
}

/// A null [`DecodeMap`].
pub(crate) enum NullDecodeMap { }
impl <A> DecodeMap<A> for NullDecodeMap {
    type Into = A;
    fn apply(a: A, _: &mut DecodeCursor<'_>, _: &DecodeContext) -> IsaResult<Self::Into> {
        Ok(a)
    }
}

/// A helper macro for simple ISAs with an opcode followed by operands.
macro_rules! isa_instruction_table {
    (@encode $ty:ty, $val:expr, $r:expr) => {
        <$ty as crate::isa::encoding::Encoder>::encode($val, $r)
    };
    (@encode_op $ty:ty, $val:expr, $r:expr) => {
        $val.__isa_encode_subtable($r)
    };
    (@parse_inner_common
        [$s1:ident $s2:ident $s3:ident $enum_name:ident ($vis:vis) $metas:tt
         [$($enum_contents:tt)*]
         $op_impl:tt
         [$($encode_impl:tt)*]
         $decode_impl:tt
         [$($len_impl:tt)*]]
        [$($rest:tt)*]
        $opcode:literal $instr:ident $add_len:literal $(($($enc:ident $name:ident: $ty:ty,)*))?
    ) => {
        isa_instruction_table!(@parse
            [
                $s1 $s2 $s3 $enum_name ($vis) $metas
                [
                    $($enum_contents)*
                    $instr $(($(<$ty as crate::isa::encoding::Encoder>::Target,)*))?,
                ]
                $op_impl
                [
                    $($encode_impl)*
                    $enum_name::$instr $(($($name,)*))? => {
                        $($(
                            isa_instruction_table!(@$enc $ty, $name, $s1)?;
                        )*)?
                    },
                ]
                $decode_impl
                [
                    $($len_impl)*
                    $enum_name::$instr $(($($name,)*))? => {
                        $add_len $($(
                            + (<$ty as crate::isa::encoding::Encoder>::instruction_len($name))
                        )*)?
                    },
                ]
            ]
            [$($rest)*]
        );
    };
    (@parse_inner
        [$s1:ident $s2:ident $s3:ident $enum_name:ident ($vis:vis) $metas:tt
         $enum_contents:tt
         [$($op_impl:tt)*]
         $encode_impl:tt
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
                [$($op_impl)* $enum_name::$instr $(($($name,)*))? => $opcode,]
                $encode_impl
                [
                    $($decode_impl)*
                    {
                        fn make_fn<M: crate::isa::encoding::DecodeMap<$enum_name>>(
                            r: &mut crate::isa::encoding::DecodeCursor<'_>,
                            ctx: &crate::isa::encoding::DecodeContext,
                        ) -> crate::isa::IsaResult<M::Into> {
                            M::apply($enum_name::$instr $(($(
                                <$ty as crate::isa::encoding::Encoder>::decode(r, ctx)?,
                            )*))?, r, ctx)
                        }
                        $s1[$s3.wrapping_add($opcode) as usize] = make_fn::<$s2>;
                    }
                ]
                $len_impl
            ]
            [$($rest)*]
            $opcode $instr 1 $(($(encode $name: $ty,)*))?
        );
    };
    (@parse_inner_subtable
        [$s1:ident $s2:ident $s3:ident $enum_name:ident ($vis:vis) $metas:tt
         $enum_contents:tt
         [$($op_impl:tt)*]
         $encode_impl:tt
         [$($decode_impl:tt)*]
         $len_impl:tt]
        [$($rest:tt)*]
        $opcode:literal $instr:ident ($subt_name:ident: $subt_ty:ty)
        ($($name:ident: $ty:ty),*)
    ) => {
        isa_instruction_table!(@parse_inner_common
            [
                $s1 $s2 $s3 $enum_name ($vis) $metas
                $enum_contents
                [
                    $($op_impl)*
                    $enum_name::$instr ($subt_name, $($name,)*) => $subt_name.__isa_op($opcode),
                ]
                $encode_impl
                [
                    $($decode_impl)*
                    {
                        struct Map<M>(M);
                        impl <M: crate::isa::encoding::DecodeMap<$enum_name>>
                            crate::isa::encoding::DecodeMap<$subt_ty> for Map<M>
                        {
                            type Into = M::Into;
                            fn apply(
                                m: $subt_ty,
                                r: &mut crate::isa::encoding::DecodeCursor<'_>,
                                ctx: &crate::isa::encoding::DecodeContext,
                            ) -> crate::isa::IsaResult<M::Into> {
                                M::apply($enum_name::$instr (m, $(
                                    <$ty as crate::isa::encoding::Encoder>::decode(r, ctx)?,
                                )*), r, ctx)
                            }
                        }
                        $s1 = <$subt_ty>::__isa_create_decode_table::<Map<$s2>>(
                            $s1, $s3.wrapping_add($opcode),
                        );
                    }
                ]
                $len_impl
            ]
            [$($rest)*]
            $opcode $instr 0 (encode_op $subt_name: $subt_ty, $(encode $name: $ty,)*)
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
    (@parse $state:tt [$opcode:literal $instr:ident(subtable $a:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner_subtable $state [$($rest)*]
            $opcode $instr (_a: $a) ()
        );
    };
    (@parse $state:tt [$opcode:literal $instr:ident(subtable $a:ty, $b:ty), $($rest:tt)*]) => {
        isa_instruction_table!(@parse_inner_subtable $state [$($rest)*]
            $opcode $instr (_a: $a) (_b: $b)
        );
    };
    (@parse
        [$s1:ident $s2:ident $s3:ident $name:ident ($vis:vis) ($(#[$meta:meta])*)
         [$($enum_contents:tt)*]
         [$($op_impl:tt)*]
         [$($encode_impl:tt)*]
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
            #[doc(hidden)]
            pub(crate) fn __isa_op(self, base: u8) -> u8 {
                base.wrapping_add(match self { $($op_impl)* })
            }
            #[doc(hidden)]
            pub(crate) fn __isa_encode_subtable(
                self, $s1: &mut crate::isa::encoding::EncodeCursor<'_>,
            ) -> crate::isa::IsaResult<()> {
                match self { $($encode_impl)* }
                Ok(())
            }
            #[allow(non_camel_case_types)]
            #[doc(hidden)]
            pub(crate) const fn __isa_create_decode_table<
                $s2: crate::isa::encoding::DecodeMap<$name>,
            >(
                mut $s1: crate::isa::encoding::DecodeTable<$s2::Into>,
                $s3: u8,
            ) -> crate::isa::encoding::DecodeTable<$s2::Into> {
                $($decode_impl)*
                $s1
            }
        }
        impl crate::isa::encoding::Encoder for $name {
            type Target = $name;
            fn encode(
                v: $name, w: &mut crate::isa::encoding::EncodeCursor<'_>,
            ) -> crate::isa::IsaResult<()> {
                w.push_byte(v.__isa_op(0));
                v.__isa_encode_subtable(w)
            }
            fn decode(
                r: &mut crate::isa::encoding::DecodeCursor<'_>,
                ctx: &crate::isa::encoding::DecodeContext,
            ) -> crate::isa::IsaResult<$name> {
                const OP_TABLE: crate::isa::encoding::DecodeTable<$name> = {
                    let table: crate::isa::encoding::DecodeTable<$name> = [
                        |_, _| Err(crate::isa::IsaError::InvalidInstruction); 256
                    ];
                    $name::__isa_create_decode_table::<crate::isa::encoding::NullDecodeMap>(
                        table, 0,
                    )
                };
                OP_TABLE[r.pull_byte()? as usize](r, ctx)
            }
            fn instruction_len(v: $name) -> u8 {
                match v { $($len_impl)* }
            }
        }
    };
    ($($(#[$meta:meta])* $vis:vis table $name:ident { $($body:tt)* })*) => {$(
        isa_instruction_table!(@parse
            [_s1 _s2 _s3 $name ($vis) ($(#[$meta])*) [] [] [] [] []]
            [$($body)*]
        );
    )*};
}