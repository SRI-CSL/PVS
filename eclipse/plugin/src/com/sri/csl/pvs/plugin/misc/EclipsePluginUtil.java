package com.sri.csl.pvs.plugin.misc;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.editor.PVSEditor;
import com.sri.csl.pvs.plugin.views.TreeNode;

public class EclipsePluginUtil {
		
	public static IEditorPart getVisibleEditor() {
		IWorkbench wb = PlatformUI.getWorkbench();
		IWorkbenchWindow win = wb.getActiveWorkbenchWindow();
		IWorkbenchPage page = win.getActivePage();
		IEditorReference[] editorReferences = page.getEditorReferences();
		for (IEditorReference edRef: editorReferences) {
			IEditorPart ed = edRef.getEditor(false);
			if ( page.isPartVisible(ed) ) {
				return ed;
			}
			
		}
		return null;
	}
	
	public static String getRelativePathOfVisiblePVSEditorFilename() {
		// It returns the relative path to the current project. 
		String filename = null;
		IEditorPart acEditor = EclipsePluginUtil.getVisibleEditor();
		if ( acEditor instanceof PVSEditor ) {
			filename = ((PVSEditor)acEditor).getFile().getProjectRelativePath().toOSString();
		}
		return filename;
	}
	
	public static String selectDirectory(String message) {
		String newL = null;
	    Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		DirectoryDialog dd = new DirectoryDialog(shell);
		dd.setMessage(message);
		newL = dd.open();
		return newL;
	}
	
	public static void showMessage(String message, int type) {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		MessageBox box = new MessageBox(shell, type);
		if ( type == SWT.ICON_ERROR ) 
			box.setText("Error");
		else if ( type == SWT.ICON_WARNING ) 
			box.setText("Warning");
		else 
			box.setText("Message");
		box.setMessage(message);
		box.open();
	}
	
	public static int askUserYesNo(String question) {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		MessageBox box = new MessageBox(shell, SWT.YES | SWT.NO);
		box.setText("Question");
		box.setMessage(question);
		return box.open();
	}
	
	public static int askUserYesNoCancel(String question) {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		MessageBox box = new MessageBox(shell, SWT.YES | SWT.NO | SWT.CANCEL);
		box.setText("Question");
		box.setMessage(question);
		return box.open();
	}
	
	public static int askUserOkCancel(String question) {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		MessageBox box = new MessageBox(shell, SWT.OK | SWT.CANCEL);
		box.setText("Question");
		box.setMessage(question);
		return box.open();
	}
	
	public static String getFilenameWithoutExtension(String filename) {
		return filename.substring(0,filename.lastIndexOf('.'));
	}
	
	public static TreeNode convertTheories2TreeNode(TreeNode parent, PVSTheory... theories) {
		if ( parent == null ) {
			parent = new TreeNode("");
		}
		if ( theories != null ) {
			for(PVSTheory theory: theories) {
				parent.addChild(new TreeNode(theory));
			}
		}
		return parent;
	}
	
}
