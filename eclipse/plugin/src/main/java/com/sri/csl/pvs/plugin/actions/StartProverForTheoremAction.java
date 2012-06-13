package com.sri.csl.pvs.plugin.actions;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.action.Action;
import org.json.JSONObject;

import com.sri.csl.pvs.PVSConstants;
import com.sri.csl.pvs.PVSException;
import com.sri.csl.pvs.PVSExecutionManager;
import com.sri.csl.pvs.PVSJsonWrapper;
import com.sri.csl.pvs.declarations.PVSDeclaration;
import com.sri.csl.pvs.declarations.PVSTheory;

public class StartProverForTheoremAction extends Action {
	protected static Logger log = Logger.getLogger(StartProverForTheoremAction.class.getName());
	
	private static final String COMMAND = "json-prove-formula";
	private static final String INPROVER = "in_prover";
	private String theory, theorem;
	private String menuText = "Start Prover";
	
	public StartProverForTheoremAction(PVSTheory ty, PVSDeclaration decl) {
		theorem = decl.getID();
		theory = ty.getID();
		setText(menuText);
		setToolTipText("Start the prover for " + theorem);
	}
	
	

	public void run() {
		log.log(Level.INFO, "Starting Prover for {0}", theorem);
		try {
			JSONObject result = (JSONObject)PVSJsonWrapper.INST().sendCommand(COMMAND, theory, theorem);
			Map<?, ?> map = result.getMap();
			if ( map.containsKey(INPROVER) ) {
				String value = map.get(INPROVER).toString();
				if ( PVSConstants.TRUE.equalsIgnoreCase(value) ) {
					PVSExecutionManager.INST().setPVSMode(PVSExecutionManager.PVSMode.PROVER);
				}
			}
		} catch (PVSException e) {
			log.log(Level.SEVERE, COMMAND + " failed: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
