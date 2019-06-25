use reon::isa::IsaResult;
use reon::isa::encoding::{InstructionType, EncodeCursor, DecodeCursor, DecodeContext};
use reon::isa::w65816::*;

#[inline(never)]
fn decode(r: &mut DecodeCursor<'_>, ctx: &DecodeContext) -> IsaResult<Instruction> {
    Instruction::decode(r, &ctx)
}

#[inline(never)]
fn encode(i: &Instruction, w: &mut EncodeCursor<'_>) -> IsaResult<()> {
    i.encode(w)
}

fn disasm(data: &[u8]) {
    let ctx = DecodeContext {
        emulation_mode: false,
        a_8_bit: false,
        xy_8_bit: false
    };
    let mut idx = 0;
    let mut cursor = DecodeCursor::new(data);
    let mut vec = Vec::new();
    while idx < data.len() {
        let instr = decode(&mut cursor, &ctx).unwrap();
        println!("$00:{:04x} {:?}", 0x8000+idx, instr);
        idx += instr.instruction_len() as usize;
        vec.push(instr);
    }

    let mut buf = Vec::new();
    let mut cursor = EncodeCursor::new(&mut buf);
    for instr in vec {
        encode(&instr, &mut cursor).unwrap();
    }

    println!();
    println!("Input and output equal: {}", buf.as_slice() == data);
    println!("Instr size: {}/{}", std::mem::size_of::<Instruction>(),
                                  std::mem::align_of::<Instruction>());
}

fn main() {
    disasm(include_bytes!("../tests/arch-65816.sfc"));
}