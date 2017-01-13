package net.slimevoid.tong;

import java.util.ArrayList;
import java.util.List;

import net.slimevoid.tong.InstrR.Op;

public class Natives {
	
	public static List<Procedure> buildNatives() {
		List<Procedure> nat = new ArrayList<>();
		nat.add(buildRedraw());
		nat.add(buildExit());
		nat.add(buildDraw());
		return nat;
	}
	
	private static Procedure buildRedraw() {
		List<Instr> instrs = new ArrayList<>();
		Register addr = Register.allocReg();
		instrs.add(new InstrR(Op.LI, 0x10000, addr));
		instrs.add(new InstrR(Op.SW, addr, addr));
		addr.free();
		return new Procedure("redraw", instrs);
	}
	
	private static Procedure buildExit() {
		List<Instr> instrs = new ArrayList<>();
		Register addr = Register.allocReg();
		instrs.add(new InstrR(Op.LI, 0x10001, addr));
		instrs.add(new InstrR(Op.SW, addr, addr));
		addr.free();
		return new Procedure("exit", instrs);
	}
	
	private static Procedure buildDraw() {
		List<Instr> instrs = new ArrayList<>();
		Register x = Register.allocReg(); //TODO add x and y args
		Register c = Register.allocReg();
		instrs.add(new InstrR(Op.SW, c, x));
		c.free();
		x.free();
		return new Procedure("draw", instrs);
	}
}
