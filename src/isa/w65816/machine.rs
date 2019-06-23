//! Types relating to the machine code representation of 65816 assembly.

use crate::isa::{Rel8, Rel16, SnesOffset};
use crate::isa::encoding::*;

isa_instruction_table! {
    /// Operand combinations for memory related instructions
    pub table MemOperand {
        0x05 Zero(u8),
        0x15 ZeroX(u8),
        0x03 Stack(u8),
        0x0D Near(u16),
        0x1D NearX(u16),
        0x19 NearY(u16),
        0x0F Far(SnesOffset),
        0x1F FarX(SnesOffset),
        0x12 IndZero(u8),
        0x01 IndZeroX(u8),
        0x11 IndYZero(u8),
        0x13 IndYStack(u8),
        0x07 FarIndZero(u8),
        0x17 FarIndYZero(u8),
    }

    /// Operand combinations for accumulator related instructions
    pub table AccOperand {
        0x00 Mem(class MemOperand),
        0x09 Imm(#imm_a),
    }

    /// Operand combinations for index ALU related instructions
    pub table AluIndexOperand {
        0x00 Imm(#imm_xy),
        0x04 Zero(u8),
        0x0C Near(u16),
    }

    /// Operand combinations for memory ALU instructions
    pub table MemAluOperand {
        0x06 Zero(u8),
        0x16 ZeroX(u8),
        0x0E Near(u16),
        0x1E NearX(u16),
    }

    /// Operand combinations for rotate/shift instructions.
    pub table RotOperand {
        0x00 Mem(class MemAluOperand),
        0x0A Acc,
    }

    /// Raw instruction representation for the 65816 ISA.
    pub table W65816MachineInstruction {
        // Transfer instructions
        0xA8 Tay,
        0xAA Tax,
        0xBA Tsx,
        0x98 Tya,
        0x8A Txa,
        0x9A Txs,
        0x9B Txy,
        0xBB Tyx,
        0x7B Tdc,
        0x5B Tcd,
        0x3B Tsc,
        0x1B Tcs,

        // Load memory to register
        0xA0 Lda(class AccOperand),
        0xA2 LdxImm(#imm_xy),
        0xA6 LdxZero(u8),
        0xB6 LdxZeroY(u8),
        0xAE LdxNear(u16),
        0xBE LdxNearY(u16),
        0xA0 LdyImm(#imm_xy),
        0xA4 LdyZero(u8),
        0xB4 LdyZeroX(u8),
        0xAC LdyNear(u16),
        0xBC LdyNearX(u16),

        // Store to memory
        0x64 StzZero(u8),
        0x74 StzZeroX(u8),
        0x9C StzNear(u16),
        0x9E StzNearX(u16),
        0x80 Sta(class MemOperand),
        0x86 StxZero(u8),
        0x96 StxZeroY(u8),
        0x8E StxNear(u16),
        0x84 StyZero(u8),
        0x94 StyZeroX(u8),
        0x8C StyNear(u16),

        // Stack opcodes
        0x48 Pha,
        0xDA Phx,
        0x5A Phy,
        0x08 Php,
        0x8B Phb,
        0x4B Phk,
        0x0B Phd,
        0xD4 Pei(u8), // PeiZero
        0xF4 Pea(u16), // PeaImm
        0x62 Per(Rel16),
        0x68 Pla,
        0xFA Plx,
        0x7A Ply,
        0x2B Pld,
        0xAB Plb,
        0x28 Plp,

        // Memory block transfer
        0x44 Mvp(u8, u8),
        0x54 Mvn(u8, u8),

        // Alu operations
        0x00 Ora(class AccOperand),
        0x20 And(class AccOperand),
        0x40 Eor(class AccOperand),
        0x60 Adc(class AccOperand),
        0xE0 Sbc(class AccOperand),
        0xC0 Cmp(class AccOperand),
        0xE0 Cpx(class AluIndexOperand),
        0xC0 Cpy(class AluIndexOperand),

        // Bit tests
        0x24 BitZero(u8),
        0x2C BitNear(u16),
        0x34 BitZeroX(u8),
        0x3C BitNearX(u16),
        0x89 BitImm(#imm_a),

        // Increment/decrement operations
        0xE0 Inc(class MemAluOperand),
        0xE8 Inx,
        0xC8 Iny,
        0x1A Ina,
        0xC0 Dec(class MemAluOperand),
        0xCA Dex,
        0x88 Dey,
        0x3A Dea,

        // TSB/TRB
        0x04 TsbZero(u8),
        0x0C TsbNear(u16),
        0x14 TrbZero(u8),
        0x1C TrbNear(u16),

        // Rotates
        0x00 Asl(class RotOperand),
        0x40 Lsr(class RotOperand),
        0x20 Rol(class RotOperand),
        0x60 Ror(class RotOperand),

        // Unconditional jumps
        0x80 Bra(Rel8),
        0x82 Brl(Rel16),
        0x4C JmpNear(u16),
        0x5C JmpFar(SnesOffset),
        0x6C JmpIndZero(u16),
        0x7C JmpIndNearX(u16),
        0xDC Jml(u16),
        0x20 JsrNear(u16),
        0x22 Jsl(SnesOffset),
        0xFC JsrIndNearX(u16),
        0x40 Rti,
        0x6B Rtl,
        0x60 Rts,

        // Conditional branches
        0x10 Bpl(Rel8),
        0x30 Bmi(Rel8),
        0x50 Bvc(Rel8),
        0x70 Bvs(Rel8),
        0x90 Bcc(Rel8),
        0xB0 Bcs(Rel8),
        0xD0 Bne(Rel8),
        0xF0 Beq(Rel8),

        // Interrupt related
        0x00 Brk(u8),
        0x02 Cop(u8),

        // CPU Control
        0x18 Clc,
        0x58 Cli,
        0xD8 Cld,
        0xB8 Clv,
        0x38 Sec,
        0x78 Sei,
        0xF8 Sed,
        0xC2 Rep(u8),
        0xE2 Sep(u8),
        0xFB Xce,

        // Special opcodes
        0xDB Stp,
        0xEB Xba,
        0xCB Wai,
        0x42 Wdm(u8),
        0xEA Nop,
    }
}