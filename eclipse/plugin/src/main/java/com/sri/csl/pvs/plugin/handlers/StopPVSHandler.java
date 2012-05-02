package com.sri.csl.pvs.plugin.handlers;

import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import com.sri.csl.pvs.PVSExecutionManager;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class StopPVSHandler extends AbstractHandler {
	private IWorkbenchWindow window;
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
		window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
		PVSExecutionManager.INST().stopPVS();
		return null;
	}
	
}