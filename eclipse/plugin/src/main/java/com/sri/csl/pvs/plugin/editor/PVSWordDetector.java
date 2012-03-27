package com.sri.csl.pvs.plugin.editor;

import org.eclipse.jface.text.rules.IWordDetector;

public class PVSWordDetector implements IWordDetector {
	/*
	 * @param c character 
	 * @return boolean the character is authorized in a word
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordPart(char)
	 */
	public boolean isWordPart(char c) {
		return c == '-' || c == '_' || c == '!'
				|| Character.isLetter(c) || Character.isDigit(c);
	}

	public boolean isWordStart(char c) {
		return Character.isLetter(c);
	}
}
