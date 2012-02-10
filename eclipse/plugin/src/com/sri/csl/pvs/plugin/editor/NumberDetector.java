package com.sri.csl.pvs.plugin.editor;

import org.eclipse.jface.text.rules.IWordDetector;

public class NumberDetector implements IWordDetector {
	private boolean sawPoint = false;
	
	public boolean isWordPart(char c) {
		if ( sawPoint && c == '.' )
			return false;
		sawPoint = (c == '.');
		return Character.isDigit(c) || sawPoint;
	}

	public boolean isWordStart(char c) {
		sawPoint = (c == '.');
		return Character.isDigit(c) || c == '+' || c == '-' || sawPoint;
	}
}
