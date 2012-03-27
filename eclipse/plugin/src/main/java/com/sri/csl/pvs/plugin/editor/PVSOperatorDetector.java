package com.sri.csl.pvs.plugin.editor;

import org.eclipse.jface.text.rules.IWordDetector;

public class PVSOperatorDetector implements IWordDetector {
	private static String operatorChar = "-+!#*:()=<>\\|[];^%,&@`{}.~";

	public boolean isWordPart(char c) {
		return operatorChar.indexOf(c)>-1;
	}

	public boolean isWordStart(char c) {
		return operatorChar.indexOf(c)>-1;
	}

}
