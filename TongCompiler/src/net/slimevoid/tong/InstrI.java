package net.slimevoid.tong;

import java.io.*;
import java.util.*;

public abstract class Instr {
	
	public abstract int toAsm(Compiler compiler);
}
