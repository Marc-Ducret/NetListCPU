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

public class Compiler {

	private static Compiler instance;
	
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
		instance = new Compiler(src);
		instance.compile(new DataOutputStream(out));
		out.close();
	}

	private final List<String> toks;
	private int index = 0;
	private Map<String, Integer> vars = new HashMap<>();
	private Map<String, Procedure> procs = new HashMap<>();

	private Compiler(String src) {
		StringBuilder build = new StringBuilder();
		List<String> toks = new ArrayList<>();
		boolean comment = false;
		for(int i = 0; i < src.length(); i++) {
			char c = src.charAt(i);
			if(('A' <= c && c <= 'z') || c == '.' || c == '_' || ('0' <= c && c <= '9'))
				build.append(c);
			else {
				if(c == '\'') {
					if(build.length() > 0) error("Missplaced '");
					if(i+2 >= src.length()) error("Missplaced '");
					char ch = src.charAt(i+1);
					i += 2;
					toks.add(""+(int) ch);
				} else {
					String s = build.toString();
					if(s.length() > 0 && !comment) toks.add(s);
					build.setLength(0);
					if(c == '#') comment = true;
					if(c != ' ' && c != '\t' && c != '\r' && c != '\n' && !comment) toks.add(""+c);
					if(c == '\n') comment = false;
				}
			}
		}
		toks.add("~EOF");
		this.toks = toks;
	}

	public static void error(String msg) {
		System.err.println("Error: "+msg+" at token "+instance.curToken());
		System.exit(-1);
	}

	private void compile(DataOutputStream out) throws IOException {
		if(!curToken().equals(".vars")) error("Program must start with .vars");
		List<String> vars = new ArrayList<>();
		String tok;
		while((tok = nextToken()) != null && !tok.startsWith(".")) {
			if(!isIdent(tok)) error("Invalid identifier: "+tok);
			if(vars.contains(tok)) error("Identifier "+tok+" is already used");
			vars.add(tok);
		}
		System.out.println("vars: ");
		int offset = 0x11000;
		for(String var : vars) {
			System.out.print(var+" ");
			this.vars.put(var, offset++);
		}			
		System.out.println();
		if(tok == null || !tok.equals(".prgm")) error("Missing .prgm");
		nextToken();
		for(Procedure p : Natives.buildNatives()) procs.put(p.name, p);
		for(Procedure proc; (proc = nextProc()) != null;) {
			if(procs.containsKey(proc.name)) error("Procedure "+proc.name+" already exists");
			procs.put(proc.name, proc);
		}
		for(Procedure p : procs.values()) {
			System.out.println("proc: "+p.name);
			int ct = 0;
			for(Instr instr : p.instrs) System.out.println(Integer.toHexString(ct++)+"\t"+instr);		
		}
		if(!procs.containsKey("main")) error("No main procedure");
		for(Instr instr : procs.get("main").instrs) out.writeInt(instr.toAsm(this));
	}

	private Procedure nextProc() {
		if(curToken().equals("~EOF")) return null;
		if(!curToken().equals("@")) error("Missing @");
		String name = nextToken();
		if(!isIdent(name)) error("Invalid identifier");
		List<Instr> instrs = new ArrayList<>();
		while(nextInstr(instrs));
		return new Procedure(name, instrs);
	}

	private boolean nextInstr(List<Instr> instrs) {
		String tok = nextToken();
		if(tok.equals("@") || tok.equals("~EOF")) return false;
		if(tok.equals(">")) {
			String func = nextToken();
			if(!procs.containsKey(func)) error("Unknown function "+func);
			List<Register> args = new ArrayList<>();
			while(!(tok = nextToken()).equals(";")) {
				Register r = Register.allocReg();
				args.add(r);
				prevToken();
				computeExpr(r, instrs);
			}
			prevToken();
			for(Register r : args) r.free();
			instrs.addAll(procs.get(func).instrs);
		} else if(tok.equals("?")) {
			Register zero = Register.allocReg();
			instrs.add(new InstrR(Op.LI, 0x0, zero));
			Register out = Register.allocReg();
			computeExpr(out, instrs);
			int start = instrs.size();
			instrs.add(null);
			out.free();
			zero.free();
			if(!nextToken().equals("{")) error("Missing {");
			while(!nextToken().equals("}")) {
				prevToken();
				if(!nextInstr(instrs)) error("Missing }");
			}
			instrs.set(start, new InstrR(Op.BEQI, instrs.size(), out, zero));
			return true;
		} else if(tok.equals("!")) {
			int start = instrs.size();
			Register zero = Register.allocReg();
			instrs.add(new InstrR(Op.LI, 0x0, zero));
			Register out = Register.allocReg();
			computeExpr(out, instrs);
			int jmp = instrs.size();
			instrs.add(null);
			out.free();
			zero.free();
			if(!nextToken().equals("{")) error("Missing {");
			while(!nextToken().equals("}")) {
				prevToken();
				if(!nextInstr(instrs)) error("Missing }");
			}
			instrs.add(new InstrR(Op.JI, start));
			instrs.set(jmp, new InstrR(Op.BEQI, instrs.size(), out, zero));
			return true;
		} else if(isIdent(tok)) {
			if(!vars.containsKey(tok)) error("Unknown variable "+tok);
			if(!nextToken().equals("=")) error("Missing =");
			Register out = Register.allocReg();
			computeExpr(out, instrs);
			instrs.add(new InstrR(Op.SWI, vars.get(tok), out));
			out.free();
		} else error("Invalid instruction");
		if(!nextToken().equals(";")) error("Missing ;");
		return true;
	}

	private void computeExpr(Register out, List<Instr> instrs) {
		String tok = nextToken();
		if(tok.equals("(")) {
			computeExpr(out, instrs);
			if(!nextToken().equals(")")) error("Missing )");
		} else if(isIdent(tok)) {
			instrs.add(new InstrR(Op.LWI, vars.get(tok), out));
		} else if(isNumber(tok)) {
			int i = tok.startsWith("0x") ? Integer.parseInt(tok.substring(2), 16): Integer.parseInt(tok);
			instrs.add(new InstrR(Op.LI, i, out));
		} else if(tok.equals("-")) {
			computeExpr(out, instrs);
			//... do
		} else error("Invalid expression");
		Operator op = nextOp();
		if(op != null) {
			Register tmp = Register.allocReg();
			computeExpr(tmp, instrs);
			Op o = null;
			switch(op) {
			case AND:
				o = Op.AND;
				break;
			case EQ:
				o = Op.SEQ;
				break;
			case LE:
				o = Op.SLE;
				break;
			case LT:
				o = Op.SLT;
				break;
			case MINUS:
				o = Op.SUB;
				break;
			case NEQ:
				o = Op.SNE;
				break;
			case OR:
				o = Op.OR;
				break;
			case PLUS:
				o = Op.ADD;
				break;
			case SL:
				o = Op.SL;
				break;
			case SR:
				o = Op.SR;
			default:
				break;
			}
			instrs.add(new InstrR(o, out, tmp));
			tmp.free();
		}
	}
	
	private Operator nextOp() {
		String tok = nextToken();
		switch(tok) {
		case "+": return Operator.PLUS;
		case "-": return Operator.MINUS;
		case "*": return Operator.SL;
		case "/": return Operator.SR;
		case "&": return Operator.AND;
		case "|": return Operator.OR;
		case "=": return Operator.EQ;
		case "!": if(!nextToken().equals("=")) error("Expected = after !");
				  return Operator.NEQ;
		case "<": if(nextToken().equals("=")) return Operator.LE;
			prevToken();
			return Operator.LT;
		default: 
			prevToken();
			return null;
		}
	}

	private String nextToken() {
		if(index >= toks.size()) index = toks.size()-1;
		return toks.get(++index);
	}

	private String curToken() {
		if(index >= toks.size()) index = toks.size()-1;
		return toks.get(index);
	}
	
	private void prevToken() {
		index--;
	}

	private boolean isIdent(String str) {
		return str.matches("[a-zA-Z_]+");
	}
	
	private boolean isNumber(String str) {
		return str.matches("(-?[0-9]+)|(0x[0-9A-F]+)");
	}
}
