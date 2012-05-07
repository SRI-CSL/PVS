package com.sri.csl.pvs.plugin.views;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.IEditorPart;

import com.sri.csl.pvs.declarations.PVSDeclaration;
import com.sri.csl.pvs.declarations.PVSDeclarationPlace;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.editor.PVSEditor;
import com.sri.csl.pvs.plugin.misc.EclipseGuiUtil;

public class PVSTheoriesTreeNodeSelectionChanged implements ISelectionChangedListener {
	private TreeViewer viewer;
	protected static Logger log = Logger.getLogger(PVSTheoriesTreeNodeSelectionChanged.class.getName());
	
	public PVSTheoriesTreeNodeSelectionChanged(TreeViewer viewer) {
		this.viewer = viewer;
	}

	@Override
	public void selectionChanged(SelectionChangedEvent event) {
		if ( !event.getSelection().isEmpty() ) {
			if (viewer.getSelection() instanceof IStructuredSelection) {
				IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
                Object obj = selection.getFirstElement();
                if ( obj instanceof TreeNode ) {
                	TreeNode node = (TreeNode)obj;
                	Object nodeObject = node.getObject();
                	if ( nodeObject instanceof PVSDeclaration ) {
                		onPVSDeclarationSelected((PVSDeclaration)nodeObject);
                	} else if ( nodeObject instanceof PVSTheory ) {
                		onPVSTheorySelected((PVSTheory)nodeObject);
                	} else {
                		onUnknownNodeSelected(nodeObject);
                	}
                }
			}
		}
	}
	
	public void onPVSDeclarationSelected(PVSDeclaration decl) {
		PVSDeclarationPlace place = decl.getPlace();
		log.log(Level.INFO, "PVSDeclaration {0} was selected ", decl.getID());
		IEditorPart ed = EclipseGuiUtil.getVisibleEditor();
		if ( ed instanceof PVSEditor ) {
			PVSEditor editor = (PVSEditor)ed;
			editor.goToLine(place.x1);
		}
	}

	public void onPVSTheorySelected(PVSTheory theory) {
		log.log(Level.INFO, "PVSTheory {0} was selected ", theory.getID());
	}

	public void onUnknownNodeSelected(Object obj) {
		log.log(Level.INFO, "Unknown node {0} was selected ", obj);
		
	}
	
	

}
