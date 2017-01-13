package net.slimevoid.sd.simulatorenv;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class FrameScreen implements Screen {

	private static final int FONT_SIZE = 16;
	
	private char[] buff;
	private JPanel pan;
	private Font font;

	public void init(int w, int h) {
		JFrame frame = new JFrame("TongScreen");
		frame.setLocationRelativeTo(null);
		frame.setVisible(true);
		font = new Font(Font.MONOSPACED, Font.PLAIN, FONT_SIZE);
		pan = new JPanel() {
			private static final long serialVersionUID = 1L;

			@Override
			public void paint(Graphics g) {
				g.setFont(font); 
				FontMetrics metrics = g.getFontMetrics(font);
				StringBuilder builder = new StringBuilder();
				if(buff != null) {
					for(int y = 0; y < h; y ++) {
						for(int x = 0; x < w; x ++) {
							builder.append(buff[x+w*y]);
						}
						g.drawString(builder.toString(), 5, FONT_SIZE+y*FONT_SIZE);
						builder.setLength(0);
					}
				} else {
					frame.setSize(metrics.stringWidth(".") * w + 10, metrics.getHeight() * h + 10);
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
