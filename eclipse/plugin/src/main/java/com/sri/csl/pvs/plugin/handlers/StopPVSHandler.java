package com.sri.csl.pvs.plugin.handlers;

import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.sri.csl.pvs.PVSExecutionManager;
import com.sri.csl.pvs.plugin.console.IOConsoleKeyboardReader;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class StopPVSHandler extends AbstractHandler {
	protected static Logger log = Logger.getLogger(StopPVSHandler.class.getName());
	
	/**
	 * The constructor.
	 */
	public StopPVSHandler() {
	}

	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {
		log.info("Message to stop PVS was received");
		//window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
		PVSExecutionManager.INST().stopPVS();
		IOConsoleKeyboardReader.INST().finish();
		return null;
	}
	
}