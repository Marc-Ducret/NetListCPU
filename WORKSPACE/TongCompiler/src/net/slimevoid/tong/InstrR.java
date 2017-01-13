package net.slimevoid.tong;

public class InstrR extends Instr {

	public static enum Op {
		
			ADD	(0x41000000), 
			SUB (0x41800000), 
			SL	(0x40040000), 
			SR	(0x40060000), 
			AND	(0x40002000), 
			OR	(0x40008000),
			XOR	(0x40004000),
			SLT	(0x41E00000),
			SLE	(0x41F00000),
			SEQ	(0x41D00000),
			SNE	(0x41D80000),
			MUL	(0x40001000),
			MOV (0x40010000),
			SW	(0x30010000),
			LW	(0x60010000),
			SWI	(0xB0000000),
			LWI	(0xE0000000),
			LI	(0xC0000000),
			J	(0x0C010000),
			JZ	(0x08010000),
			JNZ	(0x04010000),
			JI	(0x8C000000),
			JZI	(0x88000000),
			JNZI(0x84000000),
			BEQI(0x8B800000),
			BNEI(0x87800000);
		
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
