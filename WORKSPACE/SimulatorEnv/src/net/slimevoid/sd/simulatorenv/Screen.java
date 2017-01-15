package net.slimevoid.sd.simulatorenv;

import java.awt.event.KeyListener;

public interface Screen {

	public void init(int w, int h);
	public void draw(char[] buff);
	public void clear();
	public void addKeyListener(KeyListener list); //TODO use it
}
