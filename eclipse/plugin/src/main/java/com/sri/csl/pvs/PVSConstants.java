package com.sri.csl.pvs;

public class PVSConstants {
	public final static String simplePrompt = "pvs\\([0-9]+\\)";
	public final static String pvsAllegroPrompt = "^((\\[\\d+i?c?\\] |\\[step\\] )?((<?[-\\p{Alpha}]* ?\\d*>)|([-\\p{Alnum}]+\\(\\d+\\):)|Rule\\?|\\(Y or N\\)|\\(Yes or No\\)|Please enter.*:) )+";
	public final static String pvsCmuPrompt = "^\\([0-9]+\\]+\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\) \\|Rule\\? \\|<GndEval> \\|<PVSio> \\|yices > \\|(Y or N)\\|(Yes or No)\\|Please enter";

}
