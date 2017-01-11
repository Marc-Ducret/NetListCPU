package net.slimevoid.tong;

public class InstrR extends Instr {

	public static enum Op {JMP, 
			AND	(0x0000_0022), 
			OR	(0x0000_0042), XOR, COMP, LT, EQ, NEQ, SL, SR, ADD, SUB, MUL, 
			LW	(0x0100_0000), 
			SW	(0x0180_0002);
		
		private final int code;
		
		private Op() {this(0);}//TODO RM
		private Op(int code) {
			this.code = code;
		}
	}

	public final Register from, dest;
	public final Op op;

	public InstrR(Op op, Register from, Register dest) {
		this.op = op;
		this.from = from;
		this.dest = dest;
	}
	
	@Override
	public int toAsm(Compiler compiler) {
		return op.code | ((dest.ordinal() | (from.ordinal() << 3)) << 24);
	}
	
	@Override
	public String toString() {
		return op.name().toLowerCase()+" "+from+" "+dest;
	}
}
