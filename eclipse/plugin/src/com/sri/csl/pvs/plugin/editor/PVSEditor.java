package com.sri.csl.pvs.plugin.editor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.IElementStateListener;

import com.sri.csl.pvs.declarations.PVSFormula;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.views.PVSTheoriesView;
import com.sri.csl.pvs.plugin.views.TreeNode;

public class PVSEditor extends TextEditor {
	private final ColorManager colorManager;
	private IFile file;
	private boolean modelGenerated;
	private TreeNode treeModel = new TreeNode("");
	
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
				FileEditorInput fInput = (FileEditorInput) element;
				file = fInput.getFile();
			}

			public void elementDeleted(Object element) {
				file = null;
			}

			public void elementDirtyStateChanged(Object element, boolean isDirty) {
				FileEditorInput fInput = (FileEditorInput) element;
				file = fInput.getFile();
			}

			public void elementMoved(Object originalElement, Object movedElement) {
				FileEditorInput fInput = (FileEditorInput) movedElement;
				file = fInput.getFile();
			}
		});
	}
	
	public IFile getFile() {
		FileEditorInput fei = (FileEditorInput)getEditorInput();
		return fei.getFile();
	}
	
	private ArrayList<PVSTheory> getTheories() {
		ArrayList<PVSTheory> theories = new ArrayList<PVSTheory>();
		//TODO: This should be linked to actual PVS and Sam's code
		PVSTheory d1 = new PVSTheory(3, "dummy 1");
		d1.addFormula(new PVSFormula("Good Formula"));
		theories.add(d1);

		IPath path = getFile().getFullPath();
		String location = path.toOSString();
		PVSTheory d2 = new PVSTheory(3, location);
		theories.add(d2);
		
		return theories;
	}
	
	private void generatePVSModel() {
		treeModel.clear();
		for(PVSTheory theory: getTheories()) {
			treeModel.addChild(new TreeNode(theory));
		}
	}
	
	public void updatePVSTheoriesView() {
		if ( !modelGenerated ) {
			generatePVSModel();
			modelGenerated = true;
		}
		PVSTheoriesView.getInstance().setInput(treeModel);
	}
	
	
	protected void setSite(IWorkbenchPartSite site) {
		super.setSite(site);

		IWorkbenchPage page = getSite().getPage();
		page.addPartListener(new PVSEditorActivationListener());
	}
	
			
	public void dispose() {
		colorManager.dispose();
		super.dispose();
	}

	public void doSave(IProgressMonitor progressMonitor) {
		
		super.doSave(progressMonitor);
		
		modelGenerated = false;
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

	@SuppressWarnings("rawtypes")
	@Override
	public Object getAdapter(Class cls) {
		//System.out.println("Adapter Class: " + cls.getName());
		Object adapter = super.getAdapter(cls);
		//System.out.println("Adapter: " + adapter);
		return adapter;
	}
		
	private final void outputFileToPVS(Process process) throws CoreException, IOException {
		// Lecture et renvoi a PVS
		InputStream in = file.getContents();
		OutputStream out = process.getOutputStream();
		int ch = 0;
		while ((ch = in.read()) > -1) {
			out.write(ch);
		}
		out.close();
		in.close();
	}

	private static final String getOuput(InputStream errStream)
			throws IOException {
		final InputStreamReader iReader = new InputStreamReader(errStream);
		BufferedReader bReader = new BufferedReader(iReader);
		String r = "";
		String s;
		while ((s = bReader.readLine()) != null) {
			r += "|" + s;
		}
		return r;
	}

}
