package com.sri.csl.pvs;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;

import com.sri.csl.pvs.plugin.misc.EclipsePluginUtil;

/*
 * To add a new command:
 * 1) Define a new static string for the command, e.g. PARSE
 * 2) in getArguments(), implement how to gather all the necessary arguments
 * 3) in execute(), define how to run the command
 * 
 * 
 */

public class PVSCommandManager {
	private static String BUFFEDQUOTE = "\\\"";
	private static String SPACE = " ", COMMA = ", ";
	
	// PVS COMMANDS:
	private static String PARSE = "parse";
	private static String TYPECHECK = "pvs-typecheck-file";
	private static String CHANGECONTEXT = "change-context";
	
	
	public static Object handleCommand(String command) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException {
		if ( !PVSExecutionManager.isPVSRunning() ) {
			EclipsePluginUtil.showMessage("PVS is not running", SWT.ICON_ERROR);
			return null;
		}
		System.out.println("PVS Command to run: " + command);
		return execute(command, getArguments(command));
	}
	
	private static String prepare(String command, Object... args) {
		
		StringBuffer buffer = new StringBuffer("(");
		buffer.append(command).append(SPACE);
		if ( args.length > 0 ) {
			if ( args[0] instanceof String ) {
				buffer.append(BUFFEDQUOTE).append(args[0]).append(BUFFEDQUOTE);
			} else {
				buffer.append(args[0]);
			}
			for (int i=1; i<args.length; i++) {
				if ( args[i] instanceof String ) {
					buffer.append(COMMA).append(BUFFEDQUOTE).append(args[i]).append(BUFFEDQUOTE);
				} else {
					buffer.append(COMMA).append(args[i]);
				}
			}
		}
		buffer.append(")");
		return buffer.toString();
	}

	private static List<Object> getArguments(String command) {
		ArrayList<Object> args = new ArrayList<Object>();
		if ( command.equals(PARSE) ) {
			String filename = EclipsePluginUtil.getVisiblePVSEditorFilename();
			if ( filename != null ) {
				args.add(filename);
			}
		} else if ( command.equals(TYPECHECK) ) { 
			String filename = EclipsePluginUtil.getVisiblePVSEditorFilename();
			if ( filename != null ) {
				args.add(filename);
			}			
		} else if ( command.equals(CHANGECONTEXT) ) {
			String newLocation = EclipsePluginUtil.selectDirectory("Please select a new directory:");
			if ( newLocation != null ) {
				args.add(newLocation);
			}			
		}
		return args;
	}

	private static Object execute(String command, List<Object> args) {
		Object result = null;
		try {
			if ( command.equals(PARSE) ) {
				result = parse(args);
			} else if ( command.equals(TYPECHECK) ) {
				result = typecheck(args);
			} else if ( command.equals(CHANGECONTEXT) ) {
				result = changeContext(args);

			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return result;
	}
	
	private static Object parse(List<Object> args) {
		if ( args.size() == 0 ) return null;
		try {
			return PVSJsonWrapper.INST().sendCommand(PARSE, args.toArray());
		} catch (PVSException e) {
			EclipsePluginUtil.showMessage(e.getMessage(), SWT.ICON_ERROR);
		}
		return null;
	}
	
	private static Object typecheck(List<Object> args) {
		if ( args.size() == 0 ) return null;
		try {
			return PVSJsonWrapper.INST().sendCommand(TYPECHECK, args.toArray());
		} catch (PVSException e) {
			EclipsePluginUtil.showMessage(e.getMessage(), SWT.ICON_ERROR);
		}
		return null;
	}
	
	private static Object changeContext(List<Object> args) {
		if ( args.size() == 0 ) return null;
		try {
			return PVSJsonWrapper.INST().sendCommand(CHANGECONTEXT, args.toArray());
		} catch (PVSException e) {
			EclipsePluginUtil.showMessage(e.getMessage(), SWT.ICON_ERROR);
		}
		return null;
	}
	
	
}
