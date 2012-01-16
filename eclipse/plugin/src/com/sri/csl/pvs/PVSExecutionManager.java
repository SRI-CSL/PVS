package com.sri.csl.pvs;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.json.JSONException;
import org.json.JSONObject;

import com.sri.csl.pvs.plugin.Activator;
import com.sri.csl.pvs.plugin.preferences.PreferenceConstants;

public class PVSExecutionManager {
	public interface PVSRespondListener {
		public void onMessageReceived(String message);
		public void onMessageReceived(JSONObject message);
	}
	
	protected static ArrayList<PVSRespondListener> listeners = new ArrayList<PVSRespondListener>();
	protected static Process process = null;
	
	protected static String getPVSDirectory() {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		return store.getString(PreferenceConstants.P_PATH);
	}
	
	public static void addListener(PVSRespondListener l) {
		listeners.add(l);
	} 
	
	public static void removeListener(PVSRespondListener l) {
		listeners.remove(l);
	}
	
	public static String getPVSLocation() {
		return getPVSDirectory() + "/" + "pvs";
	}
	
	public static String getPVSStartingCommand() {
		return getPVSLocation()  + " -raw";
	}
	
	public static Process startPVS() throws IOException {
		if ( (new File(getPVSLocation()).exists()) ) {
			Runtime runtime = Runtime.getRuntime();
			process = runtime.exec(getPVSStartingCommand());
			
		} else {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			MessageDialog.openError(shell, "PVS Not found", "Please enter the correct path to PVS in the preference page.");
		}
		return process;
	}
	
	public static Process getProcess() {
		return process;
	}
	
	public static void writeToPVS(String message) {
		if ( process != null ) {
			OutputStream st = process.getOutputStream();
			try {
				st.write((message + "\n").getBytes());
				st.flush();
			} catch (IOException e) {
				// e.printStackTrace();
			}
		}
	}
	
	public static boolean isPVSRunning() {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		IProcess[] processes = manager.getProcesses();
		for (IProcess p: processes) {
			if ( Activator.name.equals(p.getLabel()) ) {
				return !p.isTerminated();
			}
		}
		return false;
	}
	
	public static InputStream getInputStream() {
		return process != null? process.getInputStream(): null;
	}

	public static OutputStream getOutputStream() {
		return process != null? process.getOutputStream(): null;
	}

	public static InputStream getErrorStream() {
		return process != null? process.getErrorStream(): null;
	}
	
	public static void dispatchMessage(String message) {
		JSONObject json = null;
		try {
			json = new JSONObject(message);
		} catch (JSONException e) { }

		if ( json != null ) {
			for (PVSRespondListener l: listeners) {
				l.onMessageReceived(json);
			}
		} else {
			for (PVSRespondListener l: listeners) {
				l.onMessageReceived(message);
			}							
		}
	}
	
	public static String pvsEval(String str) {
		//TODO: implement this
		writeToPVS(str);
		BufferedReader reader = new BufferedReader(new InputStreamReader(getInputStream()));
		return str;
	}

	public static void stopPVS() {
		if ( process != null ) {
			process.destroy();
		}
	}	
}
