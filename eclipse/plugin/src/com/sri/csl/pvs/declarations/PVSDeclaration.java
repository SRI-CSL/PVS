package com.sri.csl.pvs.declarations;

public abstract class PVSDeclaration {
	public static enum PVSElementKind {THEORY, CLOSED_FORMULA, TYPE};
	
	protected int position = -1;
	protected PVSElementKind kind = null;
	
	public int getPosition() {
		return position;
	}
	
	public PVSElementKind getKind() {
		return kind;
	}
	
	
}
