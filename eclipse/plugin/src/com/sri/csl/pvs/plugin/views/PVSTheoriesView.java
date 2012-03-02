package com.sri.csl.pvs.plugin.views;


import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkingSetManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.sri.csl.pvs.declarations.PVSDeclaration;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.Activator;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 */

public class PVSTheoriesView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "com.sri.csl.pvs.plugin.views.PVSTheoriesView";

	private TreeViewer viewer;
	private TreeNode invisibleRoot;
	
	private Action action1;
	private Action action2;
	private Action doubleClickAction;
	protected static Logger log = Logger.getLogger(PVSTheoriesView.class.getName());

	
	
	/*
	 * The content provider class is responsible for
	 * providing objects to the view. It can wrap
	 * existing objects in adapters or simply return
	 * objects as-is. These objects may be sensitive
	 * to the current input of the view, or ignore
	 * it and always show the same content 
	 * (like Task List, for example).
	 */
	 
	class ViewContentProvider implements ITreeContentProvider {
		
		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
			if ( newInput != null ) {
				log.log(Level.FINE, "New input received: {0}", newInput);
				if ( newInput instanceof TreeNode ) {
					invisibleRoot = (TreeNode)newInput;
				}
			} else {
				invisibleRoot = new TreeNode("");
			}
			
		}
		
		public void dispose() {
		}
		
		public Object[] getElements(Object node) {
			if (node.equals(getViewSite())) {
				return getChildren(invisibleRoot);
			}
			return getChildren(node);			
		}
		
		public Object getParent(Object child) {
			if (child instanceof TreeNode) {
				return ((TreeNode) child).getParent();
			}
			return null;
		}
		
		public Object[] getChildren(Object parent) {
			if (parent instanceof TreeNode) {
				return ((TreeNode) parent).getChildren();
			} 
			
			return new Object[0];
		}
		
		public boolean hasChildren(Object parent) {
			if (parent instanceof TreeNode)
				return ((TreeNode) parent).hasChildren();
			return false;
		}
		
		public void initialize() {
		}
	}
	
	class ViewLabelProvider extends LabelProvider implements ITableLabelProvider {
		public String getColumnText(Object obj, int index) {
			return getText(obj);
		}
		
		public Image getColumnImage(Object obj, int index) {
			return getImage(obj);
		}
		
		public Image getImage(Object obj) {
			if ( obj instanceof TreeNode ) {
				Object nodeObject = ((TreeNode)obj).getObject();
				if ( nodeObject instanceof PVSTheory ) {
					return  Activator.getImageDescriptor("icons/theory.png").createImage();
				} else if ( nodeObject instanceof PVSDeclaration ) {
					return  Activator.getImageDescriptor("icons/formula.png").createImage();
				} 
			}
			return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT);
		}
	}
	class NameSorter extends ViewerSorter {
	}

	/**
	 * The constructor.
	 */
	public PVSTheoriesView() {
		IWorkbench wb = PlatformUI.getWorkbench();
		IWorkingSetManager wsm = wb.getWorkingSetManager();
		wsm.addPropertyChangeListener(new IPropertyChangeListener() {

			@Override
			public void propertyChange(PropertyChangeEvent event) {
				log.log(Level.FINE, "Event received: {0}", event);
			}
			
		});
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent) {
		
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		viewer.setContentProvider(new ViewContentProvider());
		viewer.setLabelProvider(new ViewLabelProvider());
		viewer.setSorter(new NameSorter());
		viewer.setInput(getViewSite());

		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(viewer.getControl(), "pvs-plugin.viewer");
		makeActions();
		hookContextMenu();
		hookDoubleClickAction();
		contributeToActionBars();
	}
	
	public void setInput(Object input) {
	viewer.setInput(input);
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				PVSTheoriesView.this.fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(action1);
		manager.add(new Separator());
		manager.add(action2);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(action1);
		manager.add(action2);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(action1);
		manager.add(action2);
	}

	private void makeActions() {
		action1 = new Action() {
			public void run() {
				showMessage("Typecheck completed");
			}
		};
		action1.setText("Typecheck");
		action1.setToolTipText("Typecheck the theory");
		action1.setImageDescriptor(Activator.getImageDescriptor("icons/check.png"));
				//PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		
		action2 = new Action() {
			public void run() {
				showMessage("Proof completed");
			}
		};
		action2.setText("Prove");
		action2.setToolTipText("Prove the theory");
		action2.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		doubleClickAction = new Action() {
			public void run() {
				ISelection selection = viewer.getSelection();
				Object obj = ((IStructuredSelection)selection).getFirstElement();
				showMessage("Double-click detected on "+obj.toString());
			}
		};
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
				doubleClickAction.run();
			}
		});
	}
	private void showMessage(String message) {
		MessageDialog.openInformation(
			viewer.getControl().getShell(),
			"PVS Elements",
			message);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		viewer.getControl().setFocus();
	}
	
	public static PVSTheoriesView getInstance() {
		return (PVSTheoriesView)PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView(ID);
	}
}