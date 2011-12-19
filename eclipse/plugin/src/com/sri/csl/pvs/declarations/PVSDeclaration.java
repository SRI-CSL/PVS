package com.sri.csl.pvs.declarations;

public abstract class PVSDeclaration {
	protected int position = -1;
	protected String kind = null;
	
	public int getPosition() {
		return position;
	}
	
	public String getKind() {
		return kind;
	}
	
	
}
