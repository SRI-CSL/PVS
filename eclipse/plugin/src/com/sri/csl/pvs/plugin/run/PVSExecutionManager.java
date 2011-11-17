package com.sri.csl.pvs.plugin.run;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;

public class PVSExecutionManager {
	protected static Process process = null;
	
	protected static String getPVSLocation() {
		return "/Applications/pvs-5.0-ix86-MacOSX-allegro";
	}
	
	public static String getPVSStartingCommand() {
		return getPVSLocation() + "/" + "pvs -raw";
	}
	
	public static void startPVS() throws IOException {
		if ( !isPVSRunning() ) {
			Runtime runtime = Runtime.getRuntime();
			process = runtime.exec(getPVSStartingCommand());
		}
	}
	
	public static void stopPVS() {
		if ( isPVSRunning() ) {
			process.destroy();
			process = null;
		}
	}
	
	public static Process getProcess() {
		return process;
	}
	
	public static boolean isPVSRunning() {
		return process != null;
	}
	
	public static InputStream getInputStream() {
		return isPVSRunning()? process.getInputStream(): null;
	}

	public static OutputStream getOutputStream() {
		return isPVSRunning()? process.getOutputStream(): null;
	}

	public static InputStream getErrorStream() {
		return isPVSRunning()? process.getErrorStream(): null;
	}	
}
