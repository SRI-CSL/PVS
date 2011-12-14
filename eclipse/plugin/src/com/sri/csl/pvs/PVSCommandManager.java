package com.sri.csl.pvs;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import com.sri.csl.pvs.plugin.handlers.HandlerUtil;
import com.sri.csl.pvs.plugin.run.PVSExecutionManager;

public class PVSCommandManager {
	public static Object handleCommand(String command) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException {
		System.out.println("PVS Command to run: " + command);
		return execute(command, getArguments(command));
	}
	
	private static List<String> getArguments(String command) {
		ArrayList<String> args = new ArrayList<String>();
		if ( command.equals("parse") ) {
			args.add(HandlerUtil.getActiveFilename());
		}
		return args;
	}
	
	private static String prepare(String command, String... args) {
		String SPACE = " ", COMMA = ", ";
		StringBuffer buffer = new StringBuffer("(");
		buffer.append(command).append(SPACE);
		if ( args.length > 0 ) {
			buffer.append(args[0]);
			for (int i=1; i<args.length; i++) {
				buffer.append(COMMA).append(args[i]);
			}
		}
		buffer.append(")");
		return buffer.toString();
	}

	private static Object execute(String command, List<String> args) {
		Object result = null;
		if ( command.equals("parse") ) {
			String activeFileName = args.get(0);
			PVSExecutionManager.writeToPVS(prepare("parse ", activeFileName));
		}
		return result;
	}	
}
