package com.sri.csl.pvs.plugin.run;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.eclipse.jface.preference.IPreferenceStore;

import com.sri.csl.pvs.plugin.Activator;
import com.sri.csl.pvs.plugin.preferences.PreferenceConstants;

public class PVSExecutionManager {
	protected static Process process = null;
	
	protected static String getPVSLocation() {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		return store.getString(PreferenceConstants.P_PATH);
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
