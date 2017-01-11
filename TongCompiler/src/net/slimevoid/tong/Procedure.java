package net.slimevoid.tong;

import java.util.List;

public class Procedure {
	
	public final String name;
	public final List<Instr> instrs;
	
	public Procedure(String name, List<Instr> instrs) {
		this.name = name;
		this.instrs = instrs;
	}
}
