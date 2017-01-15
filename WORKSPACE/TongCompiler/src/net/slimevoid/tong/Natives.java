package net.slimevoid.tong;

import java.util.ArrayList;
import java.util.List;

import net.slimevoid.tong.InstrR.Op;

public class Natives {
	
	public static enum NativeVar {KEY_RES, TIME, TIME_MS}
	
	public static List<Procedure> buildNatives() {
		List<Procedure> nat = new ArrayList<>();
		nat.add(buildRedraw());
		nat.add(buildExit());
		nat.add(buildDraw());
		nat.add(buildKeyStatus());
		return nat;
	}
	
	private static Procedure buildRedraw() {
		List<Instr> instrs = new ArrayList<>();
		instrs.add(new InstrR(Op.SWI, 0x10000, Register.R0));;
		return new Procedure("redraw", instrs);
	}
	
	private static Procedure buildExit() {
		List<Instr> instrs = new ArrayList<>();
		instrs.add(new InstrR(Op.SWI, 0x10001, Register.R0));
		return new Procedure("exit", instrs);
	}
	
	private static Procedure buildDraw() {
		List<Instr> instrs = new ArrayList<>();
		Register x = Register.allocReg();
		Register c = Register.allocReg();
		instrs.add(new InstrR(Op.SW, c, x));
		c.free();
		x.free();
		return new Procedure("draw", instrs);
	}
	
	private static Procedure buildKeyStatus() {
		List<Instr> instrs = new ArrayList<>();
		Register k = Register.allocReg();
		Register addr = Register.allocReg();
		instrs.add(new InstrR(Op.LI, 0x10005, addr));
		instrs.add(new InstrR(Op.ADD, addr, k));
		instrs.add(new InstrR(Op.LW, k, addr));
		instrs.add(new InstrR(Op.SWI, 0x11000 + NativeVar.KEY_RES.ordinal(), k));
		addr.free();
		k.free();
		return new Procedure("key_status", instrs);
	}
}
