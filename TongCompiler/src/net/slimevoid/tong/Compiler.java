package net.slimevoid.tong;

import java.io.*;
import java.util.*;

public class Compiler {

	private static Compiler instance;
	
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
		OutputStream out = new BufferedOutputStream(new FileOutputStream(new File(args[1])));
		instance = new Compiler(src);
		instance.compile(out);
		out.close();
	}

	private final List<String> toks;
	private int index = 0;
	private Map<String, Integer> vars = new HashMap<>();

	private Compiler(String src) {
		StringBuilder build = new StringBuilder();
		List<String> toks = new ArrayList<>();
		for(int i = 0; i < src.length(); i++) {
			char c = src.charAt(i);
			if(('A' <= c && c <= 'z') || c == '.' || c == '_')
				build.append(c);
			else {
				String s = build.toString();
				if(s.length() > 0) toks.add(s);
				build.setLength(0);//TODO check
				if(c != ' ' && c != '\t' && c != '\r' && c != '\n') toks.add(""+c);
			}
		}
		toks.add("~EOF");
		this.toks = toks;
		for(String tok : toks) System.out.println("tok: "+tok);
	}

	public static void error(String msg) {
		System.err.println("Error: "+msg+" at token "+instance.curToken());
		System.exit(-1);
	}

	private void compile(OutputStream out) throws IOException {
		if(!nextToken().equals(".vars")) error("Program must start with .vars");
		List<String> vars = new ArrayList<>();
		String tok;
		while((tok = nextToken()) != null && !tok.startsWith(".")) {
			if(!isIdent(tok)) error("Invalid identifier: "+tok);
			vars.add(tok);
		}
		System.out.println("vars: ");
		int offset = 0xE2000;
		for(String var : vars) {
			System.out.print(var+" ");
			this.vars.put(var, offset++);
		}			
		System.out.println();
		if(tok == null || !tok.equals(".prgm")) error("Missing .prgm");
		List<Procedure> procs = new ArrayList<>();
		Procedure proc;
		while((proc = nextProc()) != null) procs.add(proc);
		if(procs.isEmpty()) error("No procedure");
		for(Procedure p : procs) {
			System.out.println("proc: "+p.name);
			for(Instr instr : p.instrs) System.out.println(instr);		
		}
	}

	private Procedure nextProc() {
		if(!nextToken().equals("@")) return null;
		String name = nextToken();
		if(!isIdent(name)) error("Invalid identifier");
		List<Instr> instrs = new ArrayList<>();
		while(nextInstr(instrs));
		return new Procedure(name, instrs);
	}

	private boolean nextInstr(List<Instr> instrs) {
		String tok = nextToken();
		if(tok.equals("@")) return false;
		if(tok.equals(">")) {
			//TODO 
		} else if(tok.equals("?")) {
			//TODO
		} else if(tok.equals("!")) {
			//TODO
		} else if(isIdent(tok)) {
			if(!vars.containsKey(tok)) error("Unknown variable "+tok);
			if(!nextToken().equals("=")) error("Missing =");
			Register out = Register.allocReg();
			computeExpr(out);
			Register addr = Register.allocReg();
			//LI addr
			//SW out addr
			out.free();
			addr.free();
		} else error("Invalid instruction");
		return true;
	}

	private void computeExpr(Register out) {
		//TODO
	}

	private String nextToken() {
		if(index >= toks.size()) index = toks.size()-1;
		return toks.get(index++);
	}

	private String curToken() {
		if(index >= toks.size()) index = toks.size()-1;
		return toks.get(index);
	}

	private boolean isIdent(String str) {
		return true; // TODO
	}
}
