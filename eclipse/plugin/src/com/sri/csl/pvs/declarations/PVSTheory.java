package com.sri.csl.pvs.declarations;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class PVSTheory {
	protected static String ID = "id";
	protected static String DECLARATIONS = "declarations";
	
	protected String id;
	protected ArrayList<PVSDeclaration> declarations = new ArrayList<PVSDeclaration>();
	
	public PVSTheory(JSONObject obj) throws JSONException {
		id = obj.getString(ID);
		if ( obj.has(DECLARATIONS) ) {
			JSONArray decls = obj.getJSONArray(DECLARATIONS);
			for (int i=0; i<decls.length(); i++) {
				PVSDeclaration decl = new PVSDeclaration(decls.getJSONObject(i));
				declarations.add(decl);
			}
		}
	}
		
	public String getID() {
		return id;
	}
	
	public void addFormula(PVSDeclaration f) {
		declarations.add(f);
	}
	
	public ArrayList<PVSDeclaration> getDeclarations() {
		return declarations;
	}

	public PVSDeclaration getDeclaration(int i) {
		return declarations.get(i);
	}

	public String toString() {
		return id;
	}
}
