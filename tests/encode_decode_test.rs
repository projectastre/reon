#![feature(test)]

extern crate test;

macro_rules! isa {
    ($isa:ident) => {
        mod $isa {
            use reon::isa::encoding::{InstructionType, EncodeCursor, DecodeCursor, DecodeContext};
            use reon::isa::$isa::*;
            pub fn decode(data: &[u8]) -> Vec<Instruction> {
                let ctx = DecodeContext {
                    emulation_mode: false,
                    a_8_bit: false,
                    xy_8_bit: false
                };

                let mut cursor = DecodeCursor::new(data);
                let mut instrs = Vec::new();
                while cursor.offset() < cursor.len() {
                    let mut offset = cursor.offset();
                    let instr = Instruction::decode(&mut cursor, &ctx).unwrap();
                    instrs.push(instr);
                    assert_eq!(cursor.offset() - offset, instr.instruction_len() as usize);
                }
                instrs
            }
            pub fn encode(instrs: &[Instruction]) -> Vec<u8> {
                let mut buf = Vec::new();
                let mut cursor = EncodeCursor::new(&mut buf);
                for instr in instrs {
                    let mut offset = cursor.offset();
                    instr.encode(&mut cursor).unwrap();
                    assert_eq!(cursor.offset() - offset, instr.instruction_len() as usize);
                }
                buf
            }
        }
    }
}
macro_rules! test {
    ($modname:ident, $isa:ident, $file:literal) => {
        mod $modname {
            use crate::$isa::*;
            use ::test::*;

            const DATA: &'static [u8] = include_bytes!($file);

            #[bench]
            fn bench_decode(b: &mut Bencher) {
                b.bytes = DATA.len() as u64;
                b.iter(|| decode(DATA));
            }

            #[bench]
            fn bench_encode(b: &mut Bencher) {
                let decoded = decode(DATA);
                b.bytes = DATA.len() as u64;
                b.iter(|| encode(&decoded));
            }

            #[test]
            fn test_encode_decode() {
                assert_eq!(DATA, encode(&decode(DATA)).as_slice());
            }
        }
    }
}

isa!(w65816);
test!(arch_65816, w65816, "arch-65816.sfc");