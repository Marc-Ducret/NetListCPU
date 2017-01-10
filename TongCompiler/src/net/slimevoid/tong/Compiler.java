package net.slimevoid.tong;

import java.io.*;
import java.util.*;

public class Compiler {
	
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
		new Compiler(src).compile(out);
		out.close();
	}

	private final String[] src;
	private int index = 0;

	private Compiler(String src) {
		this.src = src.trim().split("[ \t\n\r]+");
	}

	public void error(String msg) {
		System.err.println("Error: "+msg);
		System.exit(-1);
	}

	private void compile(OutputStream out) throws IOException {
		if(!nextToken().equals(".vars")) error("Program must start with .vars");
		List<String> vars = new ArrayList<>();
		String tok;
		while((tok = nextToken()) != null && !tok.startsWith(".")) {
			vars.add(tok);
		}
		System.out.println("vars: ");
		for(String var : vars) System.out.print(var+" ");
		System.out.println();
		if(tok == null || !tok.equals(".program")) error("Missing .program");
		List<Procedure> procs = new ArrayList<>();
		Procedure proc;
		while((proc = nextProc()) != null) procs.add(proc);
		
	}

	private Procedure nextProc() {
		if(!nextToken().equals("@")) return null;
		return null;
	}

	private Instr nextInstr() {
		if(curToken().equals("@")) return null;
		
	}

	private String nextToken() {
		if(index >= src.length) return null;
		return src[index++];
	}

	private String curToken() {
		if(index >= src.length) return null;
		return src[index];
	}
}
