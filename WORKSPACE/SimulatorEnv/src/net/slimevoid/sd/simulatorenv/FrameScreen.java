package net.slimevoid.sd.simulatorenv;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.event.KeyListener;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class FrameScreen implements Screen {

	private static final int FONT_SIZE = 16;
	
	private char[] buff;
	private JPanel pan;
	private JFrame frame;
	private Font font;
	private boolean redrawPending = false;
	private long lastRedraw = 0;
	
	private boolean sizeInit = false;

	public void init(int w, int h) {
		frame = new JFrame("TongScreen");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setLocationRelativeTo(null);
		frame.setVisible(true);
		frame.setSize(256, 256);
		font = new Font(Font.MONOSPACED, Font.PLAIN, FONT_SIZE);
		pan = new JPanel() {
			private static final long serialVersionUID = 1L;

			@Override
			public void paint(Graphics g) {
				super.paint(g);
				redrawPending = false;
				lastRedraw = System.currentTimeMillis();
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
				}
				if(!sizeInit) {
					sizeInit = true;
					frame.setSize(metrics.stringWidth(".") * w + 10, FONT_SIZE * h + 10);
				}
			}
		};
		frame.add(pan);
	}

	public void draw(char[] buff) {
		this.buff = buff;
		if(!redrawPending || (System.currentTimeMillis() - lastRedraw) > 100){
			pan.repaint();
			redrawPending = true;
		}
	}

	public void clear() {
	}

	@Override
	public void addKeyListener(KeyListener list) {
		frame.addKeyListener(list);
	}
}
