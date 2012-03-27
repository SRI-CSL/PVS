package com.sri.csl.pvs.plugin.editor;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;

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
