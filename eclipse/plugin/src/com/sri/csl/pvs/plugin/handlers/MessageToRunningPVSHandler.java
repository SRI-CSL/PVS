package com.sri.csl.pvs.plugin.handlers;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.sri.csl.pvs.PVSCommandManager;

public class MessageToRunningPVSHandler extends AbstractHandler {
	private static String PVS_PREFIX = "pvs:";

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		//String gg = HandlerUtil.getPtojectPath();
		String parameter = event.getParameter("com.sri.csl.pvs.plugin.genericCommandParameter");
		if ( parameter == null ) {
			System.out.println("Parameter " + parameter + " was not found");
			return null;
		}
		System.out.println("Message Received: " + parameter);
		Object result = null;
		if ( parameter.startsWith(PVS_PREFIX) ) {
			// The item chosen is a message to be sent to a runninb PVS:
			try {
				result = PVSCommandManager.handleCommand(parameter.substring(PVS_PREFIX.length()));
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			// The item chosen is a command that does not need be sent to PVS (like forward-theory)
			return handleGenericMessage(parameter);
		}
		return result;
	}
	
	private Object handleGenericMessage(String parameter) {		
		return null;
	}

}
