package com.sri.csl.pvs.plugin.actions;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.action.Action;

import com.sri.csl.pvs.PVSException;
import com.sri.csl.pvs.PVSJsonWrapper;
import com.sri.csl.pvs.declarations.PVSDeclaration;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.views.TreeNode;

public class ShowTccForTheoremAction extends Action {
	protected static Logger log = Logger.getLogger(ShowTccForTheoremAction.class.getName());
	
	private static final String COMMAND = "show-tcc";
	private String theory, theorem;
	private String menuText = "Show TCC";
	
	public ShowTccForTheoremAction(TreeNode formulaNode) {
		PVSDeclaration decl = ((PVSDeclaration)formulaNode.getObject());
		theorem = decl.getID();
		TreeNode parent = formulaNode.getParent();
		Object parentObject = parent.getObject();
		if ( parentObject instanceof PVSTheory ) {
			theory = ((PVSTheory)parentObject).getID();
			setText(menuText);
			setToolTipText("Show TCC for " + theorem);
			//setImageDescriptor(Activator.getImageDescriptor("icons/check.png"));
			
		} else {
			log.log(Level.SEVERE, "Parent node {0} is not a theory.", parentObject);
		}
	}
	
	

	public void run() {
		log.log(Level.INFO, "Show TCC for {0}", theorem);
		try {
			PVSJsonWrapper.INST().sendCommand(COMMAND, theorem);
		} catch (PVSException e) {
			log.log(Level.SEVERE, COMMAND + " failed: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
