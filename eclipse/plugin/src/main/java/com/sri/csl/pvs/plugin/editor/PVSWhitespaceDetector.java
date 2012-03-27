package com.sri.csl.pvs.plugin.editor;

import org.eclipse.jface.text.rules.IWhitespaceDetector;

public class PVSWhitespaceDetector implements IWhitespaceDetector {

	public boolean isWhitespace(char c) {
		return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
	}
}
