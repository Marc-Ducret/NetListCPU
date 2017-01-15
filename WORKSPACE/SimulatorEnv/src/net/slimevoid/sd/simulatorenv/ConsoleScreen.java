package net.slimevoid.sd.simulatorenv;

import java.awt.event.KeyListener;

public class ConsoleScreen implements Screen {

	private int w, h;

	public void init(int w, int h) {
		this.w = w;
		this.h = h;
	}

	public void draw(char[] buff) {
		StringBuilder builder = new StringBuilder();
		for(int y = 0; y < h; y ++) {
			for(int x = 0; x < w; x ++) {
				builder.append(buff[x+w*y]);
			}
			builder.append('\n');
		}
		System.out.print(builder.toString());
	}

	public void clear() {
		System.out.print("\033[H\033[2J");  
		System.out.flush(); 
	}

	@Override
	public void addKeyListener(KeyListener list) {
	}
}
