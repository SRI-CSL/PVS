package com.sri.csl.pvs.plugin.editor;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.editors.text.TextEditor;

import com.sri.csl.pvs.plugin.views.PVSTheoriesView;


public class PVSEditorActivationListener implements IPartListener {
	protected static Logger log = Logger.getLogger(PVSEditorActivationListener.class.getName());
	
	@Override
	public void partActivated(IWorkbenchPart part) {
		log.log(Level.INFO, "Part title is: {0}", part.getTitle());		
		PVSTheoriesView.update();
//		if ( part instanceof PVSEditor ) {
//			PVSEditor ed = (PVSEditor)part;
//			ed.updatePVSTheoriesView();
//		} else if ( part instanceof TextEditor ) {
//			PVSTheoriesView view = PVSTheoriesView.getInstance();
//			if ( view != null )
//				view.setInput(null);			
//		}
	}

	@Override
	public void partBroughtToTop(IWorkbenchPart part) {
		log.log(Level.INFO, "Part title is: {0}", part.getTitle());		
	}

	@Override
	public void partClosed(IWorkbenchPart part) {
		log.log(Level.INFO, "Part title is: {0}", part.getTitle());
	}

	@Override
	public void partDeactivated(IWorkbenchPart part) {
		log.log(Level.INFO, "Part title is: {0}", part.getTitle());		
//		if ( part instanceof PVSEditor ) {
//			PVSTheoriesView view = PVSTheoriesView.getInstance();
//			if ( view != null )
//				view.setInput(null);
//		}
	}

	@Override
	public void partOpened(IWorkbenchPart part) {
		log.log(Level.INFO, "Part title is: {0}", part.getTitle());		
	}
}
