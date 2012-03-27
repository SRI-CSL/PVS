package com.sri.csl.pvs.plugin.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.sri.csl.pvs.plugin.Activator;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class PVSPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	protected DirectoryFieldEditor pvsDirectoryEditor;
	protected BooleanFieldEditor savePVSContext;
	
	public PVSPreferencePage() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
		setDescription("PVS Preferences");
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		pvsDirectoryEditor = new DirectoryFieldEditor(PreferenceConstants.PVSPATH, "&PVS Home Directory:", getFieldEditorParent());
		savePVSContext = new BooleanFieldEditor(
				PreferenceConstants.SAVEPVSCONTEXT,
				"&Automatically set the context to the last used context when PVS starts",
				getFieldEditorParent());
		
		addField(pvsDirectoryEditor);
		addField(savePVSContext);
//
//		addField(new RadioGroupFieldEditor(
//				PreferenceConstants.P_CHOICE,
//			"An example of a multiple-choice preference",
//			1,
//			new String[][] { { "&Choice 1", "choice1" }, {
//				"C&hoice 2", "choice2" }
//		}, getFieldEditorParent()));
//		addField(
//			new StringFieldEditor(PreferenceConstants.P_STRING, "A &text preference:", getFieldEditorParent()));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
	protected void checkState() {
        super.checkState();
    	IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        String pvsDirectory = pvsDirectoryEditor.getStringValue();
        if ( pvsDirectory!= null && !pvsDirectory.equals("") ) {
        	setErrorMessage(null);
        	setValid(true);
        	store.setValue(PreferenceConstants.PVSPATH, pvsDirectory);
        } else {
        	setErrorMessage("Folder name cannot be blank!");
        	setValid(false);
        }
        
        boolean saveContext = savePVSContext.getBooleanValue();
        store.setValue(PreferenceConstants.SAVEPVSCONTEXT, saveContext);
	}
	
	public void propertyChange(PropertyChangeEvent event) {
        super.propertyChange(event);
        if ( event.getProperty().equals(FieldEditor.VALUE) ) {
        	checkState();
        }        
}	
}