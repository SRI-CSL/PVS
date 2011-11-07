package com.csl.sri.pvs.plugin.editor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.editors.text.FileDocumentProvider;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.IElementStateListener;

public class PVSEditor extends TextEditor {

	private final ColorManager colorManager;

	private IFile file;

	public PVSEditor() {
		super();
		colorManager = new ColorManager();
		setSourceViewerConfiguration(new PVSConfiguration(colorManager));
		setDocumentProvider(new PVSDocumentProvider());

		FileDocumentProvider fProvider = (FileDocumentProvider) this
				.getDocumentProvider();
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

	public void dispose() {
		colorManager.dispose();
		super.dispose();
	}

	public void doSave(IProgressMonitor progressMonitor) {

		super.doSave(progressMonitor);
		if (file == null) {
			return;
		}
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
