package net.slimevoid.tong;

import java.io.*;
import java.util.*;

public class InstrR extends Instr {

	public static enum Op {JMP, MOV, ADD, SUB, AND, OR, XOR, COMP, LT}

	public final Register from, dest;
	public final Op op;

	public InstrR(Op op, Register from, Register dest) {
		this.op = op;
		this.from = from;
		this.dest = dest;
	}
	
	@Override
	public int toAsm(Compiler compiler) {
		//TODO do
		return 0;
	}
}
