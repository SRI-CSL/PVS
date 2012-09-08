package com.sri.csl.pvs.plugin.actions;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.action.Action;
import org.json.JSONObject;

import com.sri.csl.pvs.PVSException;
import com.sri.csl.pvs.PVSJsonWrapper;

public class RunProverCommandAction extends Action {

	protected static Logger log = Logger.getLogger(RunProverCommandAction.class.getName());
	
	//TODO: COMMAND should be updated.
	private static final String COMMAND = "json-prove-formula";
	private String proverCommand;
	private String[] parameters;
	private String menuText;
	
	public RunProverCommandAction(String comm, String... params) {
		proverCommand = comm;
		parameters = params;
		menuText = comm;
		setText(menuText);
		setToolTipText(comm);		
	}
	
	

	public void run() {
		log.log(Level.INFO, "Executing prover command for {0}", proverCommand);
		try {
			JSONObject result = (JSONObject)PVSJsonWrapper.INST().sendCommand(COMMAND, proverCommand, parameters);
		} catch (PVSException e) {
			log.log(Level.SEVERE, COMMAND + " failed: " + e.getMessage());
			e.printStackTrace();
		}
	}

}
