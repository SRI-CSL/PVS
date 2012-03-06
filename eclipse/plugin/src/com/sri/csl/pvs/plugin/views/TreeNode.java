package com.sri.csl.pvs.plugin.views;

import java.util.ArrayList;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;

import com.sri.csl.pvs.declarations.PVSDeclaration;
import com.sri.csl.pvs.declarations.PVSTheory;

public class TreeNode implements IAdaptable {
	private String name;
	private Object object;
	private TreeNode parent;
	private IResource resouce;
	private ArrayList<TreeNode> children = new ArrayList<TreeNode>();
	
	
	public TreeNode(PVSTheory theory) {
		this.name = theory.getID();
		object = theory;
		for (PVSDeclaration decl: theory.getDeclarations()) {
			children.add(new TreeNode(decl));
		}
	}
	
	public TreeNode(PVSDeclaration d) {
		this.name = d.toString();
		object = d;
	}
	
	public TreeNode(String s) {
		this.name = s;
		object = s;
	}
	
	public Object getObject() {
		return object;
	}
	
	public String getName() {
		return name;
	}
	
	public void clear() {
		name = "";
		children.clear();
	}
	
	public void setParent(TreeNode parent) {
		this.parent = parent;
	}
	
	
	public TreeNode getParent() {
		return parent;
	}
	
	public String toString() {
		if ( "".equals(name) ) {
			return "!!! " + object;
		}
		return name;
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
