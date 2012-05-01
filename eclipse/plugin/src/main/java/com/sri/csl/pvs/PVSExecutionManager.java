package com.sri.csl.pvs;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISourceProviderListener;
import org.eclipse.ui.PlatformUI;
import org.json.JSONException;
import org.json.JSONObject;

import com.sri.csl.pvs.plugin.Activator;
import com.sri.csl.pvs.plugin.preferences.PreferenceConstants;
import com.sri.csl.pvs.plugin.provider.PVSStateChangeListener;

public class PVSExecutionManager implements ISourceProviderListener {
	protected static Logger log = Logger.getLogger(PVSExecutionManager.class.getName());	
	private static PVSExecutionManager instance = null;
	
	public interface PVSRespondListener {
		public void onMessageReceived(String message);
		public void onPromptReceived(List<String> previousLines, String prompt);
		public void onMessageReceived(JSONObject message);
	}
	
	public static PVSExecutionManager INST() {
		if ( instance == null ) {
			instance = new PVSExecutionManager();
		}
		return instance;
	}
	
	protected ArrayList<PVSRespondListener> respondListeners = new ArrayList<PVSRespondListener>();
	protected ArrayList<PVSStateChangeListener> stateListeners = new ArrayList<PVSStateChangeListener>();	
	protected Process process = null;
	
	protected String getPVSDirectory() {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		return store.getString(PreferenceConstants.PVSPATH);
	}
	
	public void addListener(PVSRespondListener l) {
		respondListeners.add(l);
	} 
	
	public void removeListener(PVSRespondListener l) {
		respondListeners.remove(l);
	}
	
	public void addListener(PVSStateChangeListener l) {
		stateListeners.add(l);
	} 
	
	public void removeListener(PVSStateChangeListener l) {
		stateListeners.remove(l);
	}
	
	public String getPVSLocation() {
		return getPVSDirectory() + "/" + "pvs";
	}
	
	public String getPVSStartingCommand() {
		return getPVSLocation()  + " -raw";
	}
		
	public Process startPVS() throws IOException {
		if ( (new File(getPVSLocation()).exists()) ) {
			Runtime runtime = Runtime.getRuntime();
			process = runtime.exec(getPVSStartingCommand());
			for (PVSStateChangeListener l: stateListeners) {
				l.sourceChanged(PVSConstants.PVSRUNNING, PVSConstants.TRUE);
			}
		} else {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			MessageDialog.openError(shell, "PVS Not found", "Please enter the correct path to PVS in the preference page.");
		}
		return process;
	}
	
	public Process getProcess() {
		return process;
	}
	
	public void writeToPVS(String message) {
		if ( process != null ) {
			OutputStream st = process.getOutputStream();
			try {
				st.write((message + PVSConstants.NL).getBytes());
				st.flush();
			} catch (IOException e) {
				// e.printStackTrace();
			}
		}
	}
	
	public boolean isPVSRunning() {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		IProcess[] processes = manager.getProcesses();
		for (IProcess p: processes) {
			if ( Activator.name.equals(p.getLabel()) ) {
				return !p.isTerminated();
			}
		}
		return false;
	}
	
	public InputStream getInputStream() {
		return process != null? process.getInputStream(): null;
	}

	public OutputStream getOutputStream() {
		return process != null? process.getOutputStream(): null;
	}

	public InputStream getErrorStream() {
		return process != null? process.getErrorStream(): null;
	}
	
	public void dispatchJSONMessage(String message) {
		try {
			JSONObject json = null;
			json = new JSONObject(message);
			for (PVSRespondListener l: respondListeners) {
				l.onMessageReceived(json);
			}
		} catch (JSONException e) {
			e.printStackTrace();
		}
	}
	
	public void dispatchStringMessage(String message) {
		for (PVSRespondListener l: respondListeners) {
			l.onMessageReceived(message);
		}
	}
	
	public void pvsPromptReceived(List<String> previouslines, String prompt) {
		// Prompts are dispatched just like unstructured messages for now.
		for (PVSRespondListener l: respondListeners) {
			l.onPromptReceived(previouslines, prompt);
		}		
	}

	public void stopPVS() {
		if ( process != null ) {
			process.destroy();
		}
		for (PVSStateChangeListener l: stateListeners) {
			l.sourceChanged(PVSConstants.PVSRUNNING, PVSConstants.FALSE);
		}		
	}

	@Override
	public void sourceChanged(int sourcePriority, Map sourceValuesByName) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void sourceChanged(int sourcePriority, String sourceName, Object sourceValue) {
		// TODO Auto-generated method stub
		
	}	
}
