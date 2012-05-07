package com.sri.csl.pvs.plugin.views;


import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
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
import org.eclipse.ui.IWorkingSetManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.sri.csl.pvs.PVSConstants;
import com.sri.csl.pvs.declarations.PVSDeclaration;
import com.sri.csl.pvs.declarations.PVSTheory;
import com.sri.csl.pvs.plugin.Activator;
import com.sri.csl.pvs.plugin.actions.ShowTccForTheoremAction;
import com.sri.csl.pvs.plugin.actions.StartProverForTheoremAction;
import com.sri.csl.pvs.plugin.editor.PVSEditorActivationListener;


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
	private TreeNode invisibleRoot = new TreeNode("");
	
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
				log.log(Level.INFO, "Event received: {0}", event);
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
		viewer.addSelectionChangedListener(new PVSTheoriesTreeNodeSelectionChanged(viewer));
		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(viewer.getControl(), "pvs-plugin.viewer");
		hookContextMenu();
		//hookDoubleClickAction();
		//contributeToActionBars();
		getSite().getPage().addPartListener(new PVSEditorActivationListener());
	}
	
	public void setInput(TreeNode input) {
		if ( input != null ) {
			log.log(Level.INFO, "newInput: {0}", input.getPrettyString());
			invisibleRoot = input;
		} else {
			log.info("newInput is null");
			invisibleRoot = new TreeNode("");
		}
		viewer.setInput(invisibleRoot);
	}
		
	public static void clear() {
		PVSTheoriesView view = getInstance();
		if (  view != null ) {
			view.setInput(null);
		}
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				//PVSTheoriesView.this.fillContextMenu(manager);
				fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalToolBar(bars.getToolBarManager());
	}
	
	private void fillContextMenu(IMenuManager imanager) {
    	MenuManager manager = (MenuManager)imanager;
    	Menu menu = manager.createContextMenu(viewer.getControl());
        if (viewer.getSelection().isEmpty()) {
            return;
        }

        if (viewer.getSelection() instanceof IStructuredSelection) {
            IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
            Object obj = selection.getFirstElement();
            if ( obj instanceof TreeNode ) {
            	TreeNode node = (TreeNode)obj;
            	Object nodeObject = node.getObject();
            	if ( nodeObject instanceof PVSDeclaration ) {
            		PVSDeclaration decl = ((PVSDeclaration)nodeObject);
            		if ( PVSConstants._FORMULADECL.equals(decl.getKind()) ) {
            			StartProverForTheoremAction action1 = new StartProverForTheoremAction(node);
            			manager.add(action1);
            			ShowTccForTheoremAction action2 = new ShowTccForTheoremAction(node);
            			manager.add(action2);
            		}
            	}
//                        manager.add(startProverAction);
                manager.setRemoveAllWhenShown(true);
                viewer.getControl().setMenu(menu);		
            } else {
            	log.log(Level.SEVERE, "Selected node is not TreeNode, it is: {0}", obj);
            }
        }
    }	
	
	private void fillLocalToolBar(IToolBarManager manager) {
		//manager.add(startProverAction);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		viewer.getControl().setFocus();
	}
	
	public static void update(TreeNode node) {
		PVSTheoriesView view = getInstance();
		if (  view != null ) {
			view.setInput(node);
		}
	}
	
	public static PVSTheoriesView getInstance() {
		return (PVSTheoriesView)PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView(ID);
	}
}