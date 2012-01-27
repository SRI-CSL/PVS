package com.sri.csl.pvs.declarations;

import java.util.ArrayList;

public class PVSTheory extends PVSDeclaration {
	protected String name;
	protected ArrayList<PVSFormula> formulas = new ArrayList<PVSFormula>();
	
	public PVSTheory(int pos, String n) {
		name = n;
		position = pos;
	}
	
	public String getName() {
		return name;
	}
	
	public void addFormula(PVSFormula f) {
		formulas.add(f);
	}
	
	public ArrayList<PVSFormula> getFormulas() {
		return formulas;
	}

}
