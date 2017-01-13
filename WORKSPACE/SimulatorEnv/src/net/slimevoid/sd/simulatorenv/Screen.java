package net.slimevoid.sd.simulatorenv;

public interface Screen {

	public void init(int w, int h);
	public void draw(char[] buff);
	public void clear();
}
