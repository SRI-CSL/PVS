package com.sri.csl.pvs.plugin.misc;

import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.sri.csl.pvs.PVSConstants;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.editor.PVSEditor;
import com.sri.csl.pvs.plugin.views.TreeNode;

public class EclipsePluginUtil {
	
	
	public static int getLispType() {
		return PVSConstants.ALLEGRO; //TODO: This should come from the preference page.
	}
	
	public static String getRelativePathOfVisiblePVSEditorFilename() {
		// It returns the relative path to the current project. 
		String filename = null;
		IEditorPart acEditor = EclipseGuiUtil.getVisibleEditor();
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
	
	public static String join(String[] pieces, String delimiter) {
	     StringBuffer b = new StringBuffer();
	     b.append(pieces[0]);
	     for (int i=1; i<pieces.length; i++) {
	    	 b.append(delimiter).append(pieces[i]);
	     }
	     return b.toString();
	}
}
