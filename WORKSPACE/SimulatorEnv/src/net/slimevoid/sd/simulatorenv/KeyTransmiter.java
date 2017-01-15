package net.slimevoid.sd.simulatorenv;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.io.OutputStream;

public class KeyTransmiter implements KeyListener {

	private final OutputStream out;
	public IOException deathNotice;
	
	public KeyTransmiter(OutputStream out) {
		this.out = out;
	}	
	@Override
	public void keyPressed(KeyEvent e) {
		if(isAlive()) {
			try {
				out.write(SimulatorEnv.PRESS);
				out.write(e.getKeyCode());
				out.flush();
			} catch (IOException ex) {
				die(ex);
			}
		}
	}

	@Override
	public void keyReleased(KeyEvent e) {
		if(isAlive()) {
			try {
				out.write(SimulatorEnv.RELEASE);
				out.write(e.getKeyCode());
				out.flush();
			} catch (IOException ex) {
				die(ex);
			}
		}
	}
	
	private void die(IOException e){
		System.err.println("KeyTransmitter died: "+e+" : "+e.getMessage());
		deathNotice = e;
	}

	@Override
	public void keyTyped(KeyEvent e) {}
	
	public boolean isAlive() {
		return deathNotice == null;
	}
}
