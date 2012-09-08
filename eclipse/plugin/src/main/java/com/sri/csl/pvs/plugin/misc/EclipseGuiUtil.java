package com.sri.csl.pvs.plugin.misc;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class EclipseGuiUtil {
	static private Integer result = -1;
	
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
	
	protected static Shell getShell() {
		return PlatformUI.getWorkbench().getDisplay().getActiveShell();		
	}
	
	public static int askUserYesNoCancel(final String question) {
		synchronized ( result ) {
			result = -1;
			Display.getDefault().syncExec(new Runnable() {
				public void run() {
					MessageBox box = new MessageBox(getShell(), SWT.YES | SWT.NO | SWT.CANCEL);
					box.setText("Question");
					box.setMessage(question);
					result = box.open();
				}
			});
			return result;
		}
	}
	
	public static int askUserYesNo(final String question) {
		synchronized ( result ) {
			result = -1;
			Display.getDefault().syncExec(new Runnable() {
				public void run() {
					MessageBox box = new MessageBox(getShell(), SWT.YES | SWT.NO);
					box.setText("Question");
					box.setMessage(question);
					result = box.open();
				}
			});
			return result;
		}
	}	

	public static int askUserOkCancel(final String question) {
		synchronized ( result ) {
			result = -1;
			Display.getDefault().syncExec(new Runnable() {
				public void run() {
					MessageBox box = new MessageBox(getShell(), SWT.OK | SWT.CANCEL);
					box.setText("Question");
					box.setMessage(question);
					result = box.open();
				}
			});
			return result;
		}
	}
	
	public static int askUserMultipleChoice(final String question, final List<String> choices) {
		synchronized ( result ) {
			result = -1;
			Display.getDefault().syncExec(new Runnable() {
				public void run() {
					MultipleChoiceDialog box = new MultipleChoiceDialog(getShell(), question, choices);
					result = (Integer)box.open();
				}
			});
		}		
		return result;
	}	
}
