package net.slimevoid.sd.simulatorenv;

import java.io.*;

public class SimulatorEnv {
	
	public static final int EXIT = 0, CHAR = 1, REDRAW = 2;

	public static void main(String[] args) throws IOException {
		if(args.length > 0) {
			Process p = Runtime.getRuntime().exec(args[0]);
			new SimulatorEnv(80, 20).run(p.getInputStream());
		} else {
			//TESTING start
			PipedInputStream in = new PipedInputStream();
			PipedOutputStream out = new PipedOutputStream();
			in.connect(out);
			new Thread(new Runnable() {
				
				@Override
				public void run() {
					try {
						for(int i = 0; i < 20; i ++) {
							if(i % 5 == 4) {
								out.write(2);
								Thread.sleep(500);
							}
							out.write(1);
							out.write(i);
							out.write(i);
							out.write((int)'x');
						}
						out.write(0);
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}).start();
			new SimulatorEnv(80, 20).run(in);
			//TESTING end
			System.err.println("Please specify a simulator to run");
			System.exit(-1);
		}
	}

	private final int w;
	private final int h;
	private final char[] buff;

	private SimulatorEnv(int w, int h) {
		this.w = w;
		this.h = h;
		buff = new char[w*h];
		for(int i = 0; i < buff.length; i ++) buff[i] = '.';
	}

	public void run(InputStream in) {
		try {
			in = new BufferedInputStream(in);
			while(true) {
				switch(in.read()) {
				case EXIT:
					in.close();
					return;
				
				case CHAR:
					buff[in.read() + w * in.read()] = (char) in.read();
					break;
					
				case REDRAW:
					clearScreen();
					drawScreen();
					break;
				}
			}
		} catch(Exception e) {
			System.err.println("Error while executing simulator:");
			System.err.print(e);
			if(e.getMessage() != null)
				System.err.print(": "+e.getMessage());
			System.err.println();
		}
	}

	public void clearScreen() {
		System.out.print("\033[H\033[2J");  
		System.out.flush(); 
	}

	public void drawScreen() {
		for(int y = 0; y < h; y ++) {
			for(int x = 0; x < w; x ++) {
				System.out.print(buff[x+w*y]);
			}
			System.out.println();
		}
	}

	public void handlePacket() {}
}
