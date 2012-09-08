package com.sri.csl.pvs.plugin.misc;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

class MultipleChoiceDialog extends Dialog {
	protected static Logger log = Logger.getLogger(MultipleChoiceDialog.class.getName());
	
	static String question;
	static List<String> choices;
	static int choiceMade;

	public MultipleChoiceDialog(Shell parent, String q, List<String> c) {
		super(parent);
		question = q;
		choices = c;
		choiceMade = -1;
	}
	
    public Object open() {
        Shell parent = getParent();
        Display display = parent.getDisplay();
        final Shell shell = new Shell(parent);
        createContents(shell);
        
        shell.pack();
        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) display.sleep();
        }
        return choiceMade;
    }	
    
    private void createContents(final Shell shell) {
        shell.setText("Choose one");
        RowLayout rl1 = new RowLayout();
        rl1.type = SWT.VERTICAL;
    	shell.setLayout(rl1);
        final Composite composite = new Composite(shell, getStyle());
        RowLayout rl2 = new RowLayout();
        rl2.type = SWT.VERTICAL;
        composite.setLayout(rl2);

        for (String choice: choices) {
        	Button radio = new Button(composite, SWT.RADIO);
            radio.setText(choice);
        }
    	
        Composite buttons = new Composite(shell, getStyle());
        buttons.setLayout(new RowLayout());
        
        Button ok = new Button(buttons, SWT.PUSH);
        ok.setText("OK");
        //ok.setLayoutData(data);
        ok.addSelectionListener(new SelectionAdapter() {
          public void widgetSelected(SelectionEvent event) {
    		  choiceMade = -1;
    		  int index = 0;
        	  for (Control child: composite.getChildren()) {
        		  if ( child instanceof Button ) {
        			  if ( ((Button)child).getSelection() ) {
        				  choiceMade = index;
        				  break;
        			  }
        		  }
        		  index++;
        	  }
        	  log.log(Level.INFO, "User clicked on OK and selected option {0}", index);
        	  shell.close();
          }
        });
        
        Button cancel = new Button(buttons, SWT.PUSH);
        cancel.setText("Cancel");
        cancel.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent event) {
        		log.info("User clicked on Cancel");
        		shell.close();
        	}
        });        
        
        shell.setDefaultButton(ok);
      }    
    
    
    
}
