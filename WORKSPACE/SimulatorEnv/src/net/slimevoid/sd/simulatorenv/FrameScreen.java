package net.slimevoid.sd.simulatorenv;

import java.awt.*;
import javax.swing.*;

public class FrameScreen implements Screen {

	private int w, h;
	private char[] buff;
	private JPanel pan;
	private Font font;

	public void init(int w, int h) {
		this.w = w;
		this.h = h;
		JFrame frame = new JFrame("TongScreen");
		frame.setSize(256, 256);
		frame.setLocationRelativeTo(null);
		frame.setVisible(true);
		pan = new JPanel() {
			@Override
			public void paint(Graphics g) {
				g.setFont(new Font("TimesRoman", Font.PLAIN, 12)); 
				StringBuilder builder = new StringBuilder();
				for(int y = 0; y < h; y ++) {
					for(int x = 0; x < w; x ++) {
						builder.append(buff[x+w*y]);
					}
					g.drawString(builder.toString(), 12, 12+y*12);
					builder.setLength(0);
				}
			}
		};
		frame.add(pan);
	}

	public void draw(char[] buff) {
		this.buff = buff;
		pan.repaint();
	}

	public void clear() {
		//TODO ?
	}
}
