package com.sri.csl.pvs;

import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import com.sri.csl.pvs.plugin.misc.EclipsePluginUtil;

public class PVSPromptProcessor {
	protected static Logger log = Logger.getLogger(PVSPromptProcessor.class.getName());
	
	public static void processPrompt(List<String> previousLines, String prompt) {
		int lispType = EclipsePluginUtil.getLispType();
		if ( lispType == PVSConstants.ALLEGRO ) {
			processAllegroPrompt(previousLines, prompt);
		} else if ( lispType == PVSConstants.CMU ) {
			processCMUPrompt(previousLines, prompt);
		} else {
			throw new IllegalArgumentException("lispType should be either 1 or 2");
		}
	}
	
	public static void processAllegroPrompt(List<String> previousLines, String prompt) {
	}
	
	public static void processCMUPrompt(List<String> previousLines, String prompt) {	}

	protected static String getPVSPromptRegex(int lispType) {
		switch ( lispType ) {
		case PVSConstants.ALLEGRO:
			return PVSConstants.pvsAllegroPrompt;
		case PVSConstants.CMU:
			return PVSConstants.pvsCmuPrompt;
		}
		throw new IllegalArgumentException("lispType should be either 1 or 2");
	}
	
	public static Pattern getPVSPromptPattern(int lispType) {
		Pattern p = Pattern.compile(getPVSPromptRegex(lispType));
		return p;
	}

}
