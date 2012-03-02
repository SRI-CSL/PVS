package com.sri.csl.pvs.plugin.editor;

import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.editors.text.TextEditor;

import com.sri.csl.pvs.plugin.views.PVSTheoriesView;


public class PVSEditorActivationListener implements IPartListener {
	private void updatePVSTheoriesView(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
			PVSEditor ed = (PVSEditor)part;
			ed.updatePVSTheoriesView();
		} else if ( part instanceof TextEditor ) {
			PVSTheoriesView view = PVSTheoriesView.getInstance();
//			if ( view != null )
//				view.setInput(null);			
		}		
	}
	
	@Override
	public void partActivated(IWorkbenchPart part) {
		updatePVSTheoriesView(part);
	}

	@Override
	public void partBroughtToTop(IWorkbenchPart part) {
		updatePVSTheoriesView(part);
	}

	@Override
	public void partClosed(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
			PVSEditor ed = (PVSEditor)part;
			//System.out.println("Page Closed");
		}
	}

	@Override
	public void partDeactivated(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
//			PVSTheoriesView view = PVSTheoriesView.getInstance();
//			if ( view != null )
//				view.setInput(null);			
		}
	}

	@Override
	public void partOpened(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
			//System.out.println("Page Opened");
		}
	}
	

}
