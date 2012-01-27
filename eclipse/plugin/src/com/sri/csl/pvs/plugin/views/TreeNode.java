package com.sri.csl.pvs.plugin.views;

import java.util.ArrayList;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;

import com.sri.csl.pvs.declarations.PVSFormula;
import com.sri.csl.pvs.declarations.PVSTheory;

public class TreeNode implements IAdaptable {
	private String name;
	private Object object;
	private TreeNode parent;
	private IResource resouce;
	private ArrayList<TreeNode> children = new ArrayList<TreeNode>();

	
	public TreeNode(PVSTheory theory) {
		this.name = theory.getName();
		object = theory;
		for (PVSFormula formula: theory.getFormulas()) {
			children.add(new TreeNode(formula));
		}
	}
	
	public TreeNode(PVSFormula formula) {
		this.name = formula.toString();
		object = formula;
	}
	
	public Object getObject() {
		return object;
	}
	
	public TreeNode(Object obj) {
		this.name = obj.toString();
		object = obj;
	}

	public String getName() {
		return name;
	}
	
	public void clear() {
		children.clear();
	}
	
	public void setParent(TreeNode parent) {
		this.parent = parent;
	}
	
	
	public TreeNode getParent() {
		return parent;
	}
	
	public String toString() {
		return getName();
	}
	
	
	public Object getAdapter(Class key) {
		return null;
	}
	
	protected IResource getResouce() {
		return resouce;
	}
	protected void setResouce(IResource resouce) {
		this.resouce = resouce;
	}
	
	public TreeNode[] getChildren() {
		return (TreeNode[]) children.toArray(new TreeNode[children.size()]);
	}

	public boolean hasChildren() {
		return children.size() > 0;
	}
	
	public void addChild(TreeNode child) {
		children.add(child);
		child.setParent(this);
	}		
}
