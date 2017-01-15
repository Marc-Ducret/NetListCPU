package net.slimevoid.tong;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.slimevoid.tong.InstrR.Op;

public class CompilerA {

	private static CompilerA instance;
	
	public static enum Operator {PLUS, MINUS, SL, SR, AND, OR, EQ, NEQ, LT, LE};
	
	public static void main(String[] args) throws IOException {
		if(args.length < 2) {
			System.err.println("Args format : [source file] [rom to write]");
			System.exit(-1);		
		}
		Reader read = new BufferedReader(new FileReader(new File(args[0])));
		int r;
		String src = "";
		while((r = read.read()) >= 0) {
			src += (char) r;
		}
		read.close();
		OutputStream out = new BufferedOutputStream(new FileOutputStream(new File(args[1])));
		instance = new CompilerA(src, new DataOutputStream(out));
		out.close();
	}
	
	private int index = 0;

	private CompilerA(String src, DataOutputStream out) throws IOException {
		List<Instr> instrs = new ArrayList<>();
		StringBuilder build = new StringBuilder();
		List<String> toks = new ArrayList<>();
		List<Register> registers = new ArrayList<>();
		
		registers.add(Register.R0);
		registers.add(Register.R1);
		registers.add(Register.R2);
		registers.add(Register.R3);
		registers.add(Register.R4);
		registers.add(Register.R5);
		registers.add(Register.R6);
		registers.add(Register.R7);
		
		boolean comment = false;
		for(int i = 0; i < src.length(); i++) {
			char c = src.charAt(i);
			if(('A' <= c && c <= 'z') || c == '.' || c == '_' || ('0' <= c && c <= '9'))
				build.append(c);
			else {
				String s = build.toString();
				if(s.length() > 0 && !comment) toks.add(s);
				build.setLength(0);
				if(c == '#') comment = true;
				if(c != ' ' && c != '\t' && c != '\r' && c != '\n' && !comment) toks.add(""+c);
				if(c == '\n') {
					comment = false;
					if(toks.size()==0) error("Missing line ?", ""+src.charAt(i-1));
					String opName = toks.get(0).toLowerCase();
					boolean imm=false, r1=false, r2=false;
					Op op = null;
					switch(opName) {
					case "add":
						op = Op.ADD;
						r1 = true;
						r2 = true;
						break;
					case "sub":
						op = Op.SUB;
						r1 = true;
						r2 = true;
						break;
					case "lsl":
						op = Op.SL;
						r1 = true;
						r2 = true;
						break;
					case "lsr":
						op = Op.SR;
						r1 = true;
						r2 = true;
						break;
					case "and":
						op = Op.AND;
						r1 = true;
						r2 = true;
						break;
					case "or":
						op = Op.OR;
						r1 = true;
						r2 = true;
						break;
					case "xor":
						op = Op.XOR;
						r1 = true;
						r2 = true;
						break;
					case "slt":
						op = Op.SLT;
						r1 = true;
						r2 = true;
						break;
					case "sle":
						op = Op.SLE;
						r1 = true;
						r2 = true;
						break;
					case "seq":
						op = Op.SEQ;
						r1 = true;
						r2 = true;
						break;
					case "sne":
						op = Op.SNE;
						r1 = true;
						r2 = true;
						break;
					case "mul":
						op = Op.MUL;
						r1 = true;
						r2 = true;
						break;
					case "move":
						op = Op.MOV;
						r1 = true;
						r2 = true;
						break;
					case "sw":
						op = Op.SW;
						r1 = true;
						r2 = true;
						break;
					case "lw":
						op = Op.LW;
						r1 = true;
						r2 = true;
						break;
						
					case "li":
						op = Op.LI;
						imm = true;
						r1 = true;
						break;
					case "swi":
						op = Op.SWI;
						imm = true;
						r1 = true;
						break;
					case "lwi":
						op = Op.LWI;
						imm = true;
						r1 = true;
						break;
						
					case "j":
						op = Op.J;
						r1 = true;
						break;
					case "jz":
						op = Op.JZ;
						r1 = true;
						break;
					case "jnz":
						op = Op.JNZ;
						r1 = true;
						break;
						
					case "ji":
						op = Op.JI;
						imm = true;
						break;
					case "jzi":
						op = Op.JZI;
						imm = true;
						break;
					case "jnzi":
						op = Op.JNZI;
						imm = true;
						break;
						
					case "beqi":
						op = Op.BEQI;
						imm = true;
						r1 = true;
						r2 = true;
					case "bnei":
						op = Op.BNEI;
						imm = true;
						r1 = true;
						r2 = true;
						break;
					}
					int tokInd = 1;
					int size = toks.size();
					int immV = -1;
					Register r1V = null, r2V = null;
					if(imm) {
						if(tokInd >= size) error("Missing parameter", opName);
						String tok = toks.get(tokInd);
						if(!isNumber(tok)) error("This isn't an number", tok);
						immV = tok.startsWith("0x") ? Integer.parseInt(tok.substring(2), 16): Integer.parseInt(tok);
						if(i >= (1<<17)) error("Constante trop grande", tok);
						tokInd ++;
						
					}
					if(r1) {
						if(tokInd >= size) error("Missing parameter", opName);
						String tok = toks.get(tokInd);
						if(!isNumber(tok)) error("This isn't an number", tok);
						if(Integer.parseInt(tok) > 7) error("Bad register number", tok);
						r1V = registers.get(Integer.parseInt(tok));
						tokInd ++;
					}
					if(r2) {
						if(tokInd >= size) error("Missing parameter", opName);
						String tok = toks.get(tokInd);
						if(!isNumber(tok)) error("This isn't an number", tok);
						if(Integer.parseInt(tok) > 7) error("Bad register number", tok);
						r2V = registers.get(Integer.parseInt(tok));
						tokInd ++;
					}
					if(!(tokInd==size)) error("Too much parameters", opName);
					instrs.add(new InstrR(op, immV, r1V, r2V));
					toks.clear();
				}
			}
		}
		int ct = 0;
		for(Instr instr : instrs) System.out.println(Integer.toHexString(ct++)+"\t"+instr);
		for(Instr instr : instrs) out.writeInt(instr.toAsm(this));
	}

	public static void error(String msg, String tok) {
		System.err.println("Error: "+msg+" at token "+tok);
		System.exit(-1);
	}
	
	private boolean isNumber(String str) {
		return str.matches("(-?[0-9]+)|(0x[0-9A-F]+)");
	}
}
