package com.sri.csl.pvs.declarations;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class PVSDeclaration {
	
	protected static String ID = "id";
	protected static String PLACE = "place";
	protected static String KIND = "kind";
	protected static String NONE = "none";
	
	protected String kind, id;
	protected PVSDeclarationPlace place = null;
	
	public PVSDeclaration(String i, String k) {
		id = i;
		kind = k;
	}
	
	public PVSDeclaration(JSONObject obj) throws JSONException {
		id = obj.get(ID).toString();
		kind = obj.getString(KIND);
		Object pl = obj.get(PLACE);
		if ( (pl instanceof String) && pl.toString().endsWith(NONE) ) {
			place = null;
		} else if ( pl instanceof JSONArray ) {
			JSONArray arr = ((JSONArray)pl);
			place = new PVSDeclarationPlace(arr.getInt(0), arr.getInt(1), arr.getInt(2), arr.getInt(3));
		}
	}
	
	public PVSDeclaration(String i, String k, PVSDeclarationPlace p) {
		id = i;
		kind = k;
		place = p;
	}
		
	public String getID() {
		return id;
	}
	
	public String getKind() {
		return kind;
	}
	
	public PVSDeclarationPlace getPlace() {
		return place;
	}

	public String toString() {
		return id + " (" + kind + ")";
	}
	
}
