package com.sri.csl.pvs.plugin.run;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleInputStream;

public class IOConsoleKeyboardReader extends Thread {
	private IOConsoleInputStream stream;
	private ArrayList<IOConsoleKeyboardReaderListener> listeners = new ArrayList<IOConsoleKeyboardReaderListener>();
	private boolean keepRunning;
	
	public IOConsoleKeyboardReader(IOConsole console) {
		stream = console.getInputStream();
	}
	
	public void addListener(IOConsoleKeyboardReaderListener l) {
		listeners.add(l);
	}
	
	public void removeListener(IOConsoleKeyboardReaderListener l) {
		listeners.remove(l);
	}
	
	public void finish() {
		keepRunning = false;
	}
	
	public void run() {
		BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
		String command;
		keepRunning = true;
		while ( keepRunning ) {
			try {
				command = reader.readLine();
				for (IOConsoleKeyboardReaderListener l: listeners) {
					l.onTextReceived(command);
				}
				Thread.sleep(20);
			} catch (Exception e) {
				//e.printStackTrace();
			}
		}
	}
	
	
	public interface IOConsoleKeyboardReaderListener {
		public void onTextReceived(String text);
	}
}
