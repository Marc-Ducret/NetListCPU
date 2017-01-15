package net.slimevoid.tong;

public interface Instr {
	
	public int toAsm();
	public String toTextAsm();
	public Instr shift(int offset);
}
