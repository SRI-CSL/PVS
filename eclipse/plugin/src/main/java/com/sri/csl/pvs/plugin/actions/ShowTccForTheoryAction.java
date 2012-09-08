package com.sri.csl.pvs.plugin.actions;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.action.Action;

import com.sri.csl.pvs.PVSException;
import com.sri.csl.pvs.PVSJsonWrapper;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.views.PVSTheoriesView;

public class ShowTccForTheoryAction extends Action {
	protected static Logger log = Logger.getLogger(ShowTccForTheoryAction.class.getName());
	
	private static final String COMMAND = "show-tccs";
	private String name;
	private String menuText = "Show TCC";
	
	public ShowTccForTheoryAction(PVSTheory theory) {
		name = theory.getID();
		setText(menuText);
		setToolTipText("Show TCC for " + name);
			//setImageDescriptor(Activator.getImageDescriptor("icons/check.png"));
	}
	
	

	public void run() {
		log.log(Level.INFO, "Show TCC for {0}", name);
		try {
			Object result = PVSJsonWrapper.INST().sendRawCommand(String.format("(%s \"%s\")", COMMAND, name));
			PVSTheoriesView.getInstance().setText(result.toString());
		} catch (PVSException e) {
			log.log(Level.SEVERE, COMMAND + " failed: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
