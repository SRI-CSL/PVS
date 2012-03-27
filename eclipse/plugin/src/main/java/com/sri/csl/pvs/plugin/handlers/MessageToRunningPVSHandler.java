package com.sri.csl.pvs.plugin.handlers;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.sri.csl.pvs.PVSCommandManager;

public class MessageToRunningPVSHandler extends AbstractHandler {
	private static String PVS_PREFIX = "pvs:";
	protected static Logger log = Logger.getLogger(MessageToRunningPVSHandler.class.getName());

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		//String gg = HandlerUtil.getPtojectPath();
		String parameter = event.getParameter("com.sri.csl.pvs.plugin.genericCommandParameter");
		if ( parameter == null ) {
			log.log(Level.SEVERE, "Parameter {0} was not found", parameter);
			return null;
		}
		log.log(Level.INFO, "Message Received: {0}", parameter);
		Object result = null;
		if ( parameter.startsWith(PVS_PREFIX) ) {
			// The item chosen is a message to be sent to a runninb PVS:
			result = PVSCommandManager.handleCommand(parameter.substring(PVS_PREFIX.length()));
		} else {
			// The item chosen is a command that does not need be sent to PVS (like forward-theory)
			return handleGenericMessage(parameter);
		}
		return result;
	}
	
	private Object handleGenericMessage(String parameter) {
		log.log(Level.FINE, "Generic message received: {0}", parameter);
		return null;
	}

}
