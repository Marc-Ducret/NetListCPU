package net.slimevoid.tong;

public class InstrR extends Instr {

	public static enum Op {
			ADD	(0x2200_0000), 
			SUB (0x2300_0000), 
			SL	(0x2008_0000), 
			SR	(0x200C_0000), 
			AND	(0x2000_4000), 
			OR	(0x2001_0000),
			XOR	(0x2000_8000),
			SLT	(0x23C0_0000),
			SLE	(0x23E0_0000),
			SEQ	(0x23A0_0000),
			SNE	(0x23B0_0000),
			MUL	(0x2000_0000),
			MOV (0x2002_0000),
			SW	(0x0000_3000),
			LW	(0x2000_1000),
			LI	(0xA000_0000),
			J	(0x5002_0000),
			JZ	(0x4002_0000),
			JNZ	(0x4802_0000),
			JI	(0xD000_0000),
			JZI	(0xC000_0000),
			JNZI(0xC800_0000),
			BEQI(0xC700_0000),
			BNEI(0xCF00_0000);
		
		private final int code;
		
		private Op(int code) {
			this.code = code;
		}
	}

	public final int immediate;
	public final Register r1, r2;
	public final Op op;

	public InstrR(Op op, int immediate, Register r1, Register r2) {
		this.op = op;
		this.immediate = immediate;
		this.r1 = r1;
		this.r2 = r2;
	}
	
	public InstrR(Op op, int immediate, Register r) {
		this(op, immediate, r, null);
	}
	
	public InstrR(Op op, Register r1, Register r2) {
		this(op, -1, r1, r2);
	}
	
	public InstrR(Op op, int immediate) {
		this(op, immediate, null, null);
	}

	@Override
	public int toAsm(Compiler compiler) {
		int code = op.code;
		if(immediate >= 0) 	code |= immediate << 6;
		if(r1 != null)		code |= r1.ordinal() << 3;
		if(r2 != null) 		code |= r2.ordinal();
		return code;
	}
	
	@Override
	public String toString() {
		return op.name().toLowerCase()+"\t"+(immediate >= 0 ? Integer.toHexString(immediate)+"\t" : "")
				+(r1 != null ? r1+"\t" : "")+(r2 != null ? r2+" " : "");
	}
}
