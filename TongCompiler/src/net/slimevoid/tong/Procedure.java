package net.slimevoid.tong;

import java.io.*;
import java.util.*;

public class Procedure {
	
	public final String name;
	public final Instr[] instrs;
	
	public Procedure(String name, Instr[] instrs) {
		this.name = name;
		this.instrs = instrs;
	}
}
