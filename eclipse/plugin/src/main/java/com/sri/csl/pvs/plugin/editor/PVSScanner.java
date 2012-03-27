package com.sri.csl.pvs.plugin.editor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;

public class PVSScanner extends RuleBasedScanner {

	private static String[] fgKeywords = { "AND", "CONJECTURE", "FACT", "LET", "TABLE",
		"ANDTHEN", "CONTAINING", "FALSE", "LIBRARY", "THEN", "ARRAY", "CONVERSION",
		"FORALL", "MACRO", "THEOREM", "ASSUMING", "CONVERSION+", "FORMULA", "MEASURE",
		"THEORY", "ASSUMPTION", "CONVERSION-", "FROM", "NONEMPTY_TYPE", "TRUE", "AUTO_REWRITE",
		"COROLLARY", "FUNCTION", "NOT", "TYPE", "AUTO_REWRITE+", "DATATYPE", "HAS_TYPE", "O",
		"TYPE+", "AUTO_REWRITE-", "ELSE", "IF", "OBLIGATION", "VAR", "AXIOM", "ELSIF", "IFF",
		"OF", "WHEN", "BEGIN", "END", "IMPLIES", "OR", "WHERE", "BUT", "ENDASSUMING", "IMPORTING",
		"ORELSE", "WITH", "BY", "ENDCASES", "IN", "POSTULATE", "XOR", "CASES", "ENDCOND", "INDUCTIVE",
		"PROPOSITION", "CHALLENGE", "ENDIF", "JUDGEMENT", "RECURSIVE", "CLAIM","ENDTABLE", "LAMBDA",
		"SUBLEMMA", "CLOSURE", "EXISTS", "LAW", "SUBTYPES", "COND", "EXPORTING", "LEMMA", "SUBTYPE_OF"};

	//private static String[] fgTypes = {};

	private static String[] fgConstants = {}; 

	private static String[] fgOperators = { "#", "*", ":)", "=>", "\\/", "/=", "|=",
		"##", "**", "::", ">", "|>", "#)", "+", ":=", ">=", "]|", "|[", "#]", "++",
		";", ">>", "^", "|]", "%", ",", "<", ">>=", "^^", "||", "&", "-", "<<",
		"@", "`", "|}", "&&", "->", "<<=", "@@", ".", "<=", "{|", "~", "(#", "/",
		"<=>", "[#", "{||}", "(:", "//", "<>", "[]", "|","(|", "<|", "[|",
		"|)","(||)", "/\\", "=", "[||]", "|-", ":", "==", "\\", "|->",}; 
		//"(", ")", "[", "]", "{", "}" };

	public PVSScanner(ColorManager manager) {
		IToken keyword = new Token(new TextAttribute(manager.getColor(ColorManager.KEYWORD)));
		IToken constant = new Token(new TextAttribute(manager.getColor(ColorManager.CONSTANT)));
		IToken operator = new Token(new TextAttribute(manager.getColor(ColorManager.OPERATOR)));
		IToken comment = new Token(new TextAttribute(manager.getColor(ColorManager.COMMENT)));
		IToken other = new Token(new TextAttribute(manager.getColor(ColorManager.DEFAULT)));
		//IToken type = new Token(new TextAttribute(manager.getColor(ColorManager.TYPE)));

		List<IRule> rules = new ArrayList<IRule>();

		// Add rule for single line comments beginning with %
		rules.add(new EndOfLineRule("%", comment));

		// Add rule for strings and character constants.
		rules.add(new SingleLineRule("\"", "\"", constant, '\\'));
		WordRule numberRule = new WordRule(new NumberDetector(), constant);
		rules.add(numberRule);

		// Add generic whitespace rule.
		rules.add(new WhitespaceRule(new PVSWhitespaceDetector()));

		// Add word rule for keywords, types, constants and operators.
		WordRule wordRule = new WordRule(new PVSWordDetector(), other, true);
		for (String word: fgKeywords)  wordRule.addWord(word, keyword);
		for (String word: fgConstants) wordRule.addWord(word, constant);
		//for (String word: fgTypes) wordRule.addWord(word, type);
		rules.add(wordRule);

		//TODO: Operators do not always get highlighted correctly. For now it is ok.
		WordRule operatorRule = new WordRule(new PVSOperatorDetector(), other);
		for (String word: fgOperators) operatorRule.addWord(word, operator);
		rules.add(operatorRule);

		// Convert to array
		IRule[] result = new IRule[rules.size()];
		rules.toArray(result);
		setRules(result);
	}
}
