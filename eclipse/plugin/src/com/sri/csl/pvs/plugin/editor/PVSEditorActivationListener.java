package com.sri.csl.pvs.plugin.editor;

import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;


public class PVSEditorActivationListener implements IPartListener {
	
	@Override
	public void partActivated(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
			PVSEditor ed = (PVSEditor)part;
			System.out.println("Page Activated: " + ed.getPartName());
			ed.updatePVSTheoriesView();
		}
	}

	@Override
	public void partBroughtToTop(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
			PVSEditor ed = (PVSEditor)part;
			System.out.println("Page Brought To Top: " + ed.getTitle());
			ed.updatePVSTheoriesView();
		}
	}

	@Override
	public void partClosed(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
			//System.out.println("Page Closed");
		}
	}

	@Override
	public void partDeactivated(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
			//System.out.println("Page Deactivated");
		}
	}

	@Override
	public void partOpened(IWorkbenchPart part) {
		if ( part instanceof PVSEditor ) {
			//System.out.println("Page Opened");
		}
	}
	

}
