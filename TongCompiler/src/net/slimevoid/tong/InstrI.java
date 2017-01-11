package net.slimevoid.tong;

public class InstrI extends Instr {
	
	public final Register dest;
	public final int immediate;
	
	public InstrI(Register dest, int i) {
		this.dest = dest;
		this.immediate = i % (1 << 17);
	}
	
	public int toAsm(Compiler compiler) {
		return 1 | (immediate << 8) | (dest.ordinal() << 25);
	}
	
	@Override
	public String toString() {
		return "li "+Integer.toHexString(immediate)+" "+dest;
	}
}
