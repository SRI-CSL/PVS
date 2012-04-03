package com.sri.csl.pvs.plugin.handlers;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.Launch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.handlers.HandlerUtil;
import org.json.JSONObject;

import com.sri.csl.pvs.PVSCommandManager;
import com.sri.csl.pvs.PVSConstants;
import com.sri.csl.pvs.PVSException;
import com.sri.csl.pvs.PVSExecutionManager;
import com.sri.csl.pvs.PVSExecutionManager.PVSRespondListener;
import com.sri.csl.pvs.PVSJsonWrapper;
import com.sri.csl.pvs.PVSPromptProcessor;
import com.sri.csl.pvs.plugin.Activator;
import com.sri.csl.pvs.plugin.console.IOConsoleKeyboardReader;
import com.sri.csl.pvs.plugin.console.PVSConsole;
import com.sri.csl.pvs.plugin.misc.EclipsePluginUtil;
import com.sri.csl.pvs.plugin.preferences.PreferenceConstants;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class StartPVSHandler extends AbstractHandler {
	private IOConsoleKeyboardReader keyboardReader;
	private IWorkbenchWindow window;
	protected static Logger log = Logger.getLogger(StartPVSHandler.class.getName());
	
	/**
	 * The constructor.
	 */
	public StartPVSHandler() {
	}

	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {
		log.fine("Message to start PVS was received");
		window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
		if ( PVSExecutionManager.isPVSRunning() ) {
			MessageDialog.openInformation(window.getShell(), "PVS Running", "An instance of PVS is already running.");
		} else {
			try {
				final PVSConsole console = PVSConsole.getConsole();
				console.activate();
				console.clearConsole();
				final IOConsoleOutputStream outStream = console.newOutputStream();
				Map<String, String> attributes = new HashMap<String, String>();
				attributes.put(IProcess.ATTR_CMDLINE, PVSExecutionManager.getPVSStartingCommand());
				ILaunch launch = new Launch(null, ILaunchManager.RUN_MODE, null);
				IProcess process = DebugPlugin.newProcess(launch, PVSExecutionManager.startPVS(), Activator.name, attributes);
				DebugPlugin.getDefault().getLaunchManager().addLaunch(launch);
				PVSJsonWrapper.init();
				PVSExecutionManager.addListener(new PVSRespondListener() {

					@Override
					public void onMessageReceived(String message) {
						log.log(Level.FINE, "Message received: {0}", message);
						try {
							outStream.write(message);
						} catch (IOException e) {
							e.printStackTrace();
						}
					}

					@Override
					public void onMessageReceived(JSONObject message) {
						log.log(Level.INFO, "JSON received: {0}", message);
						PVSJsonWrapper.INST().addToJSONQueue(message);
					}

					@Override
					public void onPromptReceived(List<String> previousLines, String prompt) {
						log.log(Level.INFO, "Prompt received: {0}", prompt);
						try {
							outStream.write(prompt);
						} catch (IOException e) {
							e.printStackTrace();
						}
						PVSPromptProcessor.processPrompt(previousLines, prompt);
					}
					 
				});				
				IStreamsProxy streamProxy = process.getStreamsProxy();
				IStreamMonitor outMonitor = streamProxy.getOutputStreamMonitor();
				outMonitor.addListener(new PVSStreamListener(EclipsePluginUtil.getLispType()));
				
				keyboardReader = new IOConsoleKeyboardReader(console);
				keyboardReader.addListener(new IOConsoleKeyboardReader.IOConsoleKeyboardReaderListener() {
					public void onTextReceived(String text) {
						PVSExecutionManager.writeToPVS(text);
					}
				});
				keyboardReader.start();
				Thread.sleep(500);
				restorePVSContext();
			} catch (IOException e) {
				log.severe("Failed to start PVS");
				MessageDialog.openInformation(window.getShell(), "Error", "Failed to start PVS");
			} catch (InterruptedException e) {
				log.severe("Failed to restore PVS context");
				MessageDialog.openInformation(window.getShell(), "Error", "Failed to restore the PVS context");
			} catch (PVSException e) {
				log.severe("Failed to restore PVS context");
				MessageDialog.openInformation(window.getShell(), "Error", "Failed to restore the PVS context");
			}
		}
		return null;
	}
	
	
	protected void restorePVSContext() throws PVSException {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		boolean restoreContext = store.getBoolean(PreferenceConstants.SAVEPVSCONTEXT);
		if ( restoreContext ) {
			String contextLocation = store.getString(PreferenceConstants.PVSCONTEXTPATH);
			if ( !"".equals(contextLocation) ) {
				ArrayList<Object> args = new ArrayList<Object>();
				args.add(contextLocation);
				args.add(false);
				PVSCommandManager.changeContext(args);
			}
		}
	}
}

class PVSStreamListener implements IStreamListener {
	private static Pattern pvsPromptPattern;
	private static String LCB = "{", RCB = "}";
	private static ArrayList<String> bufferedLines = new ArrayList<String>();
	StringBuffer jsonBuffer;
	boolean jsonStarted = false;
	protected static Logger log = Logger.getLogger(PVSStreamListener.class.getName());
	
	public PVSStreamListener(int lispType) {
		pvsPromptPattern = PVSPromptProcessor.getPVSPromptPattern(lispType);
		bufferedLines.clear();
		resetJSONBuffer();
	}
	
	private void resetJSONBuffer() {
		jsonStarted = false;
		jsonBuffer = new StringBuffer();
	}
	
	@Override
	public void streamAppended(String text, IStreamMonitor monitor) {
		log.log(Level.FINER, "Text was received: {0}", text);
		synchronized ( jsonBuffer ) {
			String[] lines = text.split(PVSConstants.NL);
			for (String line: lines) {
				if ( LCB.equals(line) ) {
					if ( jsonStarted ) {
						log.severe("Got another {, but did not expect it");
						resetJSONBuffer();
					} else {
						jsonStarted = true;
						jsonBuffer.append(line).append(PVSConstants.NL);
					}
				} else if ( RCB.equals(line) ) {
					if ( !jsonStarted ) {
						log.severe("Got and }, but did not expect it");
						resetJSONBuffer();
					} else {
						
						jsonBuffer.append(line).append(PVSConstants.NL);
						String jbs = jsonBuffer.toString();
						PVSExecutionManager.dispatchJSONMessage(jbs);
						resetJSONBuffer();
					}
				} else if ( !jsonStarted ) {
					if ( pvsPromptPattern.matcher(line).matches() ) {
						synchronized ( bufferedLines ) {
							PVSExecutionManager.pvsPromptReceived(bufferedLines, line);
							bufferedLines.clear();
						}
					} else { // line is unstructured data
						if ( !"nil".equals(line) ) { // nil is the result of sending a JSON to PVS. For now let's ignore and not display them
							bufferedLines.add(line);
							PVSExecutionManager.dispatchStringMessage(line + PVSConstants.NL);
						}
					}
				} else {
					jsonBuffer.append(line).append(PVSConstants.NL);
				}
			}
		}
	}
	
}