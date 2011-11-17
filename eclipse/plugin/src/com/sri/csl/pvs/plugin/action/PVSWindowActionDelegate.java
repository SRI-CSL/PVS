package com.sri.csl.pvs.plugin.action;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.Launch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.console.IOConsoleOutputStream;

import com.sri.csl.pvs.plugin.run.IOConsoleKeyboardReader;
import com.sri.csl.pvs.plugin.run.PVSConsole;
import com.sri.csl.pvs.plugin.run.PVSExecutionManager;

/**
 * Our sample action implements workbench action delegate.
 * The action proxy will be created by the workbench and
 * shown in the UI. When the user tries to use the action,
 * this delegate will be created and execution will be 
 * delegated to it.
 * @see IWorkbenchWindowActionDelegate
 */
public class PVSWindowActionDelegate implements IWorkbenchWindowActionDelegate {
	private IWorkbenchWindow window;
	private IOConsoleKeyboardReader keyboardReader;
	
	
	/**
	 * The constructor.
	 */
	public PVSWindowActionDelegate() {
	}

	/**
	 * The action has been activated. The argument of the
	 * method represents the 'real' action sitting
	 * in the workbench UI.
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(IAction action) {	
		try {
			final PVSConsole console = PVSConsole.getConsole();
			console.activate();
			console.clearConsole();
			final IOConsoleOutputStream outStream = console.newOutputStream();
			PVSExecutionManager.startPVS();
			Map<String, String> attributes = new HashMap<String, String>();
			attributes.put(IProcess.ATTR_CMDLINE, PVSExecutionManager.getPVSStartingCommand());
			ILaunch launch = new Launch(null, ILaunchManager.RUN_MODE, null);
			IProcess process = DebugPlugin.newProcess(launch, PVSExecutionManager.getProcess(), "pvs", attributes);
			IStreamsProxy streamProxy = process.getStreamsProxy();
			IStreamMonitor outMonitor = streamProxy.getOutputStreamMonitor();
			outMonitor.addListener(new IStreamListener() {

				@Override
				public void streamAppended(String arg0, IStreamMonitor arg1) {
					try {
						outStream.write(arg0);
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}				
			});
			
			keyboardReader = new IOConsoleKeyboardReader(console);
			keyboardReader.addListener(new IOConsoleKeyboardReader.IOConsoleKeyboardReaderListener() {
				public void onTextReceived(String text) {
					try {
						System.out.println("Command entered: " + text);
						Process process = PVSExecutionManager.getProcess();
						if ( process != null ) {
							OutputStream st = process.getOutputStream();
							st.write((text + "\n").getBytes());
							st.flush();
						}
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			});
			keyboardReader.start();
			
			
		} catch (IOException e) {
			MessageDialog.openInformation(window.getShell(), "Error", "Failed to start PVS");
		}		
	}
	

	/**
	 * Selection in the workbench has been changed. We 
	 * can change the state of the 'real' action here
	 * if we want, but this can only happen after 
	 * the delegate has been created.
	 * @see IWorkbenchWindowActionDelegate#selectionChanged
	 */
	public void selectionChanged(IAction action, ISelection selection) {
	}

	/**
	 * We can use this method to dispose of any system
	 * resources we previously allocated.
	 * @see IWorkbenchWindowActionDelegate#dispose
	 */
	public void dispose() {
		keyboardReader.finish();
		PVSExecutionManager.stopPVS();
	}

	/**
	 * We will cache window object in order to
	 * be able to provide parent shell for the message dialog.
	 * @see IWorkbenchWindowActionDelegate#init
	 */
	public void init(IWorkbenchWindow window) {
		this.window = window;
	}
}