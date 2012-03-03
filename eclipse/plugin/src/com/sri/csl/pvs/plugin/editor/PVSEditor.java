package com.sri.csl.pvs.plugin.editor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.IElementStateListener;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.sri.csl.pvs.PVSException;
import com.sri.csl.pvs.PVSExecutionManager;
import com.sri.csl.pvs.PVSJsonWrapper;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.misc.EclipsePluginUtil;
import com.sri.csl.pvs.plugin.views.PVSTheoriesView;
import com.sri.csl.pvs.plugin.views.TreeNode;

public class PVSEditor extends TextEditor {
	private final ColorManager colorManager;
	private boolean modelGenerated;
	private TreeNode treeModel = new TreeNode("");
	private static TreeNode emptyTreeModel = new TreeNode("");
	private static TreeNode notTypecheckedTreeModel = new TreeNode("Theories will be displayed after the file is successfully typechecked");
	protected static Logger log = Logger.getLogger(PVSEditor.class.getName());
	protected static HashMap<String, Object> typecheckedFiles = new HashMap<String, Object>();
	
	public PVSEditor() {
		super();
		modelGenerated = false;
		colorManager = new ColorManager();
		setSourceViewerConfiguration(new PVSConfiguration(colorManager));
		setDocumentProvider(new PVSDocumentProvider());

		PVSDocumentProvider fProvider = (PVSDocumentProvider)getDocumentProvider();
		fProvider.addElementStateListener(new IElementStateListener() {

			public void elementContentAboutToBeReplaced(Object element) {
			}

			public void elementContentReplaced(Object element) {
			}

			public void elementDeleted(Object element) {
			}

			public void elementDirtyStateChanged(Object element, boolean isDirty) {
				//FileEditorInput fInput = (FileEditorInput) element;
			}

			public void elementMoved(Object originalElement, Object movedElement) {
			}
		});
	}
	
	public static void setTypechecked(String filename, Object obj) {
		typecheckedFiles.put(filename, obj);
	}

	public void setTypechecked(Object obj) {
		modelGenerated = true;
		typecheckedFiles.put(EclipsePluginUtil.getFilenameWithoutExtension(getLocation()), obj);
	}
	
	public boolean isTypechecked() {
		String location = EclipsePluginUtil.getFilenameWithoutExtension(getLocation());
//		if ( isDirty() ) {
//			typecheckedFiles.remove(location);
//			return false;
//		}
		return typecheckedFiles.containsKey(location);
	}
	
	public void invalidatePVSModel() {
		modelGenerated = false;
		String location = EclipsePluginUtil.getFilenameWithoutExtension(getLocation());
		typecheckedFiles.remove(location);
	}
	
	
	/**
	 * 
	 * @return the file that is opened in this editor
	 */
	public IFile getFile() {
		FileEditorInput fei = (FileEditorInput)getEditorInput();
		return fei.getFile();
	}
	
	public String getLocation() {
		IPath path = getFile().getProjectRelativePath();
		String location = path.toOSString();
		return location;
	}
	
	public ArrayList<PVSTheory> getTheories() {
		String location = EclipsePluginUtil.getFilenameWithoutExtension(getLocation());
		if ( typecheckedFiles.containsKey(location) ) {
			return (ArrayList<PVSTheory>)typecheckedFiles.get(location);
		}
		if ( !PVSExecutionManager.isPVSRunning() ) {
			return null;
		}
		if ( !isTypechecked() ) {
			return null;
		}
		try {
			Object result = PVSJsonWrapper.INST().sendRawCommand("(json-all-theories-info \"" + location + "\")");
			return PVSJsonWrapper.getTheories((JSONObject)result);
		} catch (PVSException e) {
			log.log(Level.SEVERE, "Problem getting the theories back from PVS");
			e.printStackTrace();
		}
		log.severe("The code should not reach this");
		return null;
	}
	
	private void generatePVSModel() {
		log.info("Generating the PVS Model");
		treeModel.clear();
		ArrayList<PVSTheory> theories = getTheories();
		log.log(Level.INFO, "Theories: {0}", theories);
		if ( theories != null ) {
			for(PVSTheory theory: theories) {
				treeModel.addChild(new TreeNode(theory));
			}
		} else {
			treeModel.addChild(notTypecheckedTreeModel);						
		}
	}
	
	
	public void updatePVSTheoriesView() {
		if ( !modelGenerated ) {
			generatePVSModel();
			modelGenerated = true;
		}
		PVSTheoriesView view = PVSTheoriesView.getInstance();
		if ( view != null )
			log.log(Level.INFO, "Updating the PVS Theories View Model for {0} with {1}", new Object[] {getLocation(), treeModel});
			view.setInput(treeModel);
	}
	
	public void clearPVSTheoriesView() {
		PVSTheoriesView view = PVSTheoriesView.getInstance();
		if ( view != null )
			view.clear();
	}
				
	public void dispose() {
		colorManager.dispose();
		super.dispose();
	}

	public void doSave(IProgressMonitor progressMonitor) {
		
		super.doSave(progressMonitor);
		
		invalidatePVSModel();
		updatePVSTheoriesView();
		
		/*
		try {

			Process process = Runtime.getRuntime().exec("pvs ");
			outputFileToPVS(process);

			// Remove previous errors
			file.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);

			// Read output and display result if Ok
			InputStream outStream = process.getInputStream();
			String rOk = getOuput(outStream).replace('|', '\n').trim();
			if (rOk.length() > 0) {
				MessageDialog.openInformation(null, "PVS Output", rOk);
			}
			// Read error outputs
			InputStream errStream = process.getErrorStream();
			String error = getOuput(errStream);
			if (error.startsWith("|Error")) {
				// Quick and dirty PVS output
				String errorMessage = error.substring(error.indexOf("["));
				int i1 = error.indexOf("line(") + 5;
				int i2 = error.indexOf(")");
				String line = error.substring(i1, i2);
				int i3 = error.indexOf("position(") + 9;
				String p = error.substring(i3);
				p = p.substring(0, p.indexOf(")")).trim();

				errorMessage = errorMessage.substring(
						errorMessage.indexOf(":") + 1).trim();
				// Create the Error marker
				IMarker m = file.createMarker(IMarker.PROBLEM);
				m.setAttribute(IMarker.MESSAGE, errorMessage);
				m.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
				m.setAttribute(IMarker.LINE_NUMBER, Integer.valueOf(line));
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		*/
	}
}
