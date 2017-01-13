package net.slimevoid.tong;

public enum Register {
	R0, R1, R2, R3, R4, R5, R6, R7;

	private boolean alloc = false;
	
	public void free() {
		alloc = false;
	}

	public static Register allocReg() {
		for(Register r : values())
			if(!r.alloc) {
				r.alloc = true;
				return r;			
			}
		Compiler.error("Too few registers");
		return null;
	}
}
