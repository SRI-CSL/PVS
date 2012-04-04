package com.sri.csl.pvs;

public class PVSConstants {
	public static String NL = System.getProperty("line.separator");
	
	public final static int ALLEGRO = 1, CMU = 2;
	
	public final static String yesNo1 = "\\(Y or N\\)";
	public final static String yesNo2 = "\\(Yes or No\\)";
	private final static String numberInSquareBrackets = "(\\[\\d+i?c?\\] |\\[step\\] )";
	private final static String packageNameAndNumber = "((<?[-\\p{Alpha}]* ?\\d*>)|([-\\p{Alnum}]+\\(\\d+\\):)";
	
	public final static String pvsAllegroDebugPrompt = numberInSquareBrackets + packageNameAndNumber + ") ";
	
	public final static String pvsAllegroPrompt = "^(" + numberInSquareBrackets + "?" + packageNameAndNumber + "|Rule\\?|" + yesNo1 + "|" + yesNo2 + "|Please enter.*:) )+";
	public final static String pvsCmuPrompt = "^\\([0-9]+\\]+\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\) \\|Rule\\? \\|<GndEval> \\|<PVSio> \\|yices > \\|" + yesNo1 + "|" + yesNo2 + "|Please enter";

	public final static String pvsErrorMessageOptionIndex = " [0-9]+: ";
	
	
	// PVS COMMANDS:
	public final static String _CONTINUE = ":continue ";
}
