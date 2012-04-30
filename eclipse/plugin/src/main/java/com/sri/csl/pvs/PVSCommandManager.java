package com.sri.csl.pvs;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IEditorPart;
import org.json.JSONObject;

import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.Activator;
import com.sri.csl.pvs.plugin.editor.PVSEditor;
import com.sri.csl.pvs.plugin.misc.EclipseGuiUtil;
import com.sri.csl.pvs.plugin.misc.EclipsePluginUtil;
import com.sri.csl.pvs.plugin.preferences.PreferenceConstants;
import com.sri.csl.pvs.plugin.views.TreeNode;

/*
 * To add a new command:
 * 1) Define a new static string for the command, e.g. PARSE
 * 2) in getArguments(), implement how to gather all the necessary arguments
 * 3) in execute(), define how to run the command
 * 
 * 
 */

public class PVSCommandManager {	
	// PVS COMMANDS:
	private static String PARSE = "parse";
	private static String TYPECHECK = "typecheck-file";
	private static String CHANGECONTEXT = "change-context";
	protected static Logger log = Logger.getLogger(PVSCommandManager.class.getName());
	
	
	public static Object handleCommand(String command) {
		log.log(Level.INFO, "Handling command: {0}", command);
		if ( !PVSExecutionManager.isPVSRunning() ) {
			EclipseGuiUtil.showMessage("PVS is not running", SWT.ICON_ERROR);
			log.warning("PVS is not running");
			return null;
		}
		return execute(command, getArguments(command));
	}

	private static List<Object> getArguments(String command) {
		ArrayList<Object> args = new ArrayList<Object>();
		if ( command.equals(PARSE) ) {
			String filename = EclipsePluginUtil.getRelativePathOfVisiblePVSEditorFilename();
			if ( filename != null ) {
				args.add(filename);
			}
		} else if ( command.equals(TYPECHECK) ) { 
			String filename = EclipsePluginUtil.getRelativePathOfVisiblePVSEditorFilename();
			if ( filename != null ) {
				args.add(EclipsePluginUtil.getFilenameWithoutExtension(filename));
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
		} catch (PVSException e) {
			EclipseGuiUtil.showMessage(e.getMessage(), SWT.ICON_ERROR);
		}
		
		return result;
	}
	
	private static Object performCommandAfterVerifyingArguments(String command, List<Object> args) throws PVSException {
		return PVSJsonWrapper.INST().sendCommand(command, args.toArray());
	}
	
	private static void verifyArgumentNumbers(String command, List<Object> args, int expected) throws PVSException {
		if ( args.size() < expected ) {
			throw new PVSException("Expected number of arguments for " + command + " is at least " + expected);
		}
	}
	
	private static Object parse(List<Object> args) throws PVSException {
		verifyArgumentNumbers(PARSE, args, 1);
		return performCommandAfterVerifyingArguments(PARSE, args);

	}
	
	private static Object typecheck(List<Object> args) throws PVSException {
		verifyArgumentNumbers(TYPECHECK, args, 1);
		Object result = performCommandAfterVerifyingArguments(TYPECHECK, args);
		String file = args.get(0).toString();

		ArrayList<PVSTheory> theories = PVSJsonWrapper.getTheories(getDeclarations(file));
		TreeNode node = EclipsePluginUtil.convertTheories2TreeNode(null, theories.toArray(new PVSTheory[0]));		
		PVSEditor.setModel(file, node);
		IEditorPart ed = EclipseGuiUtil.getVisibleEditor();
		if ( ed instanceof PVSEditor ) {
			PVSEditor editor = (PVSEditor)ed;
			editor.updatePVSTheoriesView();
		}
		return result;
	}

	private static JSONObject getDeclarations(String file) throws PVSException {
		Object result = PVSJsonWrapper.INST().sendRawCommand("(json-all-theories-info \"" + file + "\")");
		return (JSONObject)result;
	}
	
	public static Object changeContext(List<Object> args) throws PVSException {
		verifyArgumentNumbers(CHANGECONTEXT, args, 1);
		Object result = performCommandAfterVerifyingArguments(CHANGECONTEXT, args);
		if ( args.size()<2 || ((Boolean)args.get(1)).booleanValue() ) { // The 2nd arg is a boolean determinging whether the context should be saved. If it is not provided, then the context is saved by default.
			String newContext = args.get(0).toString();
			IPreferenceStore store = Activator.getDefault().getPreferenceStore();
			store.setValue(PreferenceConstants.PVSCONTEXTPATH, newContext);
		}
		return result;
	}
	
	
}
