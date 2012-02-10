package com.sri.csl.pvs.plugin.handlers;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
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
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.handlers.HandlerUtil;
import org.json.JSONObject;

import com.sri.csl.pvs.PVSExecutionManager;
import com.sri.csl.pvs.PVSExecutionManager.PVSRespondListener;
import com.sri.csl.pvs.PVSJsonWrapper;
import com.sri.csl.pvs.plugin.Activator;
import com.sri.csl.pvs.plugin.console.IOConsoleKeyboardReader;
import com.sri.csl.pvs.plugin.console.PVSConsole;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class StartPVSHandler extends AbstractHandler {
	private IOConsoleKeyboardReader keyboardReader;
	private IWorkbenchWindow window;
	
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
						try {
							outStream.write(message);
						} catch (IOException e) {
							e.printStackTrace();
						}
					}

					@Override
					public void onMessageReceived(JSONObject message) {
						System.out.println("JSON received: " + message);
						PVSJsonWrapper.INST().addToJSONQueue(message);
					}
					
				});				
				IStreamsProxy streamProxy = process.getStreamsProxy();
				IStreamMonitor outMonitor = streamProxy.getOutputStreamMonitor();
				outMonitor.addListener(new PVSStreamListener(0));
				
				keyboardReader = new IOConsoleKeyboardReader(console);
				keyboardReader.addListener(new IOConsoleKeyboardReader.IOConsoleKeyboardReaderListener() {
					public void onTextReceived(String text) {
						PVSExecutionManager.writeToPVS(text);
					}
				});
				keyboardReader.start();
				
				
			} catch (IOException e) {
				MessageDialog.openInformation(window.getShell(), "Error", "Failed to start PVS");
			}
		}
		return null;
	}
}

class PVSStreamListener implements IStreamListener {
	private static String pvsAllegroPrompt = "^[ ]*\\(\\[[0-9]+i?c?\\] \\|\\[step\\] \\)?\\(\\(<[-A-Za-z]* ?[0-9]*>\\)\\3?\\|[-A-Za-z0-9]+([0-9]+): \\)\\|Rule\\? \\|<GndEval> \\|<PVSio> \\|yices > \\|(Y or N)\\|(Yes or No)\\|Please enter";
	private static String pvsCmuPrompt = "^\\([0-9]+\\]+\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\) \\|Rule\\? \\|<GndEval> \\|<PVSio> \\|yices > \\|(Y or N)\\|(Yes or No)\\|Please enter";
	private static Pattern pvsPromptPattern;
	private static String NL = "\n", LCB = "{", RCB = "}";
	StringBuffer jsonBuffer;
	boolean jsonStarted = false;
	
	public PVSStreamListener(int lispType) {
		switch ( lispType ) {
		case 0:// ALLEGRO:
			pvsPromptPattern = Pattern.compile(pvsAllegroPrompt);
			break;
		case 1:// CMU:
			pvsPromptPattern = Pattern.compile(pvsCmuPrompt);
			break;
		}
		resetJSONBuffer();
	}
	
	private void resetJSONBuffer() {
		jsonStarted = false;
		jsonBuffer = new StringBuffer();
	}
	
	@Override
	public void streamAppended(String text, IStreamMonitor monitor) {
		synchronized ( jsonBuffer ) {
			String[] lines = text.split(NL);
			for (String line: lines) {
				if ( LCB.equals(line) ) {
					if ( jsonStarted ) {
						System.err.println("Got another {, but did not expect it");
						resetJSONBuffer();
					} else {
						jsonStarted = true;
						jsonBuffer.append(line).append(NL);
					}
				} else if ( RCB.equals(line) ) {
					if ( !jsonStarted ) {
						System.err.println("Got and }, but did not expect it");
						resetJSONBuffer();
					} else {
						
						jsonBuffer.append(line).append(NL);
						PVSExecutionManager.dispatchJSONMessage(jsonBuffer.toString());
						resetJSONBuffer();
					}
				} else if ( !jsonStarted ) {
					if ( pvsPromptPattern.matcher(line).find() ) { // line is the prompt
						PVSExecutionManager.pvsPromptReceived(line);
					} else { // line is unstructured data
						PVSExecutionManager.dispatchStringMessage(line + NL);
					}
				} else {
					jsonBuffer.append(line).append(NL);
				}
			}
		}
	}
	
}