package com.sri.csl.pvs.plugin.views;

import java.util.logging.Logger;

import org.eclipse.jface.action.MenuManager;

import com.sri.csl.pvs.PVSConstants;
import com.sri.csl.pvs.PVSExecutionManager;
import com.sri.csl.pvs.declarations.PVSDeclaration;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.actions.RunProverCommandAction;
import com.sri.csl.pvs.plugin.actions.ShowTccForTheoryAction;
import com.sri.csl.pvs.plugin.actions.StartProverForTheoremAction;

public class ContextMenuFactory {
	protected static Logger log = Logger.getLogger(ContextMenuFactory.class.getName());
	
	public static void fillMenuForTreeNode(MenuManager manager, TreeNode node, PVSExecutionManager.PVSMode mode) {
    	Object nodeObject = node.getObject();
    	if ( nodeObject instanceof PVSDeclaration ) {
    		PVSDeclaration decl = ((PVSDeclaration)nodeObject);
    		TreeNode parent = node.getParent();
    		Object parentObject = parent.getObject();
    		if ( parentObject instanceof PVSTheory ) {
    			PVSTheory theory = (PVSTheory)parentObject;
    			fillMenuForDeclaration(manager, theory, decl, mode);
    		} else {
    			log.severe("Theorem has no parent Theory");
    		}
    	} else if ( nodeObject instanceof PVSTheory ) {
    		PVSTheory theory = ((PVSTheory)nodeObject);
    		fillMenuForTheory(manager, theory, mode);
    	}		
	}
	
	private static void fillMenuForDeclaration(MenuManager manager, PVSTheory theory, PVSDeclaration theorem, PVSExecutionManager.PVSMode mode) {
		if ( mode == PVSExecutionManager.PVSMode.LISP ) {			
			fillMenuForDeclarationLispMode(manager, theory, theorem);
		} else if ( mode == PVSExecutionManager.PVSMode.PROVER ) {
			fillMenuForDeclarationProverMode(manager, theory, theorem);
		} else {
			log.severe("PVSMode is OFF");
		}	
	}
	
	private static void fillMenuForTheory(MenuManager manager, PVSTheory theory, PVSExecutionManager.PVSMode mode) {
		if ( mode == PVSExecutionManager.PVSMode.LISP ) {
			fillMenuForTheoryLispMode(manager, theory);
		} else if ( mode == PVSExecutionManager.PVSMode.PROVER ) {
			fillMenuForTheoryProverMode(manager, theory);
		} else {
			log.severe("PVSMode is OFF");
		}			
	}
	
	
	
	
	private static void fillMenuForDeclarationLispMode(MenuManager manager, PVSTheory theory, PVSDeclaration decl) {
		if ( PVSConstants._FORMULADECL.equals(decl.getKind()) ) { 
			StartProverForTheoremAction action1 = new StartProverForTheoremAction(theory, decl);
			manager.add(action1);
		}		
	}
	
	private static void fillMenuForDeclarationProverMode(MenuManager manager, PVSTheory theory, PVSDeclaration theorem) {
		RunProverCommandAction grindActin = new RunProverCommandAction("grind");
		manager.add(grindActin);
		RunProverCommandAction inductActin = new RunProverCommandAction("induct");
		manager.add(inductActin);
	}
	
	private static void fillMenuForTheoryLispMode(MenuManager manager, PVSTheory theory) {
		ShowTccForTheoryAction action2 = new ShowTccForTheoryAction(theory);
		manager.add(action2);		
	}
	
	private static void fillMenuForTheoryProverMode(MenuManager manager, PVSTheory theory) {
		
	}
	
	
}
