package com.csl.sri.pvs.plugin.editor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.rules.*;
import org.eclipse.jface.text.*;

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

	private static String[] fgTypes = { "bitvector", "bool", "datatype",
			"define-type", "int", "lambda", "nat", "number", "real", "record",
			"scalar", "subrange", "subtype", "tuple" };

	private static String[] fgConstants = { "false", "true" }; 
	// May add reals and ints one day

	private static String[] fgOperators = { "#", "*", ":)", "=>", "\\/", "|=",
		"##", "**", "::", ">", "]", "|>", "#)", "+", ":=", ">=", "]|", "|[",
		"#]", "++", ";", ">>", "^", "|]", "%", ",", "<", ">>=", "^^", "||",
		"&", "-", "<<", "@", "`", "|}", "&&", "->", "<<=", "@@", "{", "}",
		"(", ".", "<=", "[", "{|", "~", "(#", "/", "<=>", "[#", "{||}",
		"(:", "//", "<>", "[]", "|", "(|", "/=", "<|", "[|", "|)",
		"(||)", "/\\", "=", "[||]", "|-", ")", ":", "==", "\\", "|->" };

	public PVSScanner(ColorManager manager) {
		IToken keyword = new Token(new TextAttribute(manager
				.getColor(ColorManager.KEYWORD)));
		IToken type = new Token(new TextAttribute(manager
				.getColor(ColorManager.TYPE)));
		IToken constant = new Token(new TextAttribute(manager
				.getColor(ColorManager.CONSTANT)));
		IToken operator = new Token(new TextAttribute(manager
				.getColor(ColorManager.OPERATION)));
		IToken comment = new Token(new TextAttribute(manager
				.getColor(ColorManager.COMMENT)));
		IToken other = new Token(new TextAttribute(manager
				.getColor(ColorManager.DEFAULT)));

		List<IRule> rules = new ArrayList<IRule>();

		// Add rule for single line comments beginning with %
		rules.add(new EndOfLineRule("%", comment));

		// Add rule for strings and character constants.
		rules.add(new SingleLineRule("\"", "\"", constant, '\\'));

		// Add generic whitespace rule.
		rules.add(new WhitespaceRule(new PVSWhitespaceDetector()));

		// Add word rule for keywords, types, constants and operators.
		WordRule wordRule = new WordRule(new PVSWordDetector(), other);
		for (int i = 0; i < fgKeywords.length; i++)
			wordRule.addWord(fgKeywords[i], keyword);
		for (int i = 0; i < fgTypes.length; i++)
			wordRule.addWord(fgTypes[i], type);
		for (int i = 0; i < fgConstants.length; i++)
			wordRule.addWord(fgConstants[i], constant);
		for (int i = 0; i < fgOperators.length; i++)
			wordRule.addWord(fgOperators[i], operator);
		rules.add(wordRule);

		// Convert to array
		IRule[] result = new IRule[rules.size()];
		rules.toArray(result);
		setRules(result);
	}
}
