package com.csl.sri.pvs.plugin.editor;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.rules.*;

public class PVSTagScanner extends RuleBasedScanner {

	public PVSTagScanner(ColorManager manager) {

		IToken string = new Token(new TextAttribute(manager
				.getColor(ColorManager.CONSTANT)));

		IRule[] rules = new IRule[2];

		// Add rule for double quotes - a double quote containing a double 
		// quote should be permited
		rules[0] = new SingleLineRule("\"", "\"", string, '\\');

		// Add generic whitespace rule - how to recognize spaces
		rules[1] = new WhitespaceRule(new PVSWhitespaceDetector());

		setRules(rules);
	}
}
