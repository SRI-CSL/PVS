package com.sri.csl.pvs;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;

import com.sri.csl.pvs.plugin.misc.EclipseGuiUtil;
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
		if ( prompt.indexOf(PVSConstants.yesNo1)>-1 ) {
			int respond = EclipseGuiUtil.askUserYesNoCancel(prompt);
			switch ( respond ) {
			case SWT.YES:
				PVSExecutionManager.writeToPVS("Y");
				break;
			case SWT.NO:
				PVSExecutionManager.writeToPVS("N");
				break;
			case SWT.CANCEL:
				break;
			}
		} else if ( prompt.indexOf(PVSConstants.yesNo2)>-1 ) {
			int respond = EclipseGuiUtil.askUserYesNoCancel(prompt);
			switch ( respond ) {
			case SWT.YES:
				PVSExecutionManager.writeToPVS("Yes");
				break;
			case SWT.NO:
				PVSExecutionManager.writeToPVS("No");
				break;
			case SWT.CANCEL:
				break;
			}
		} else if ( prompt.matches(PVSConstants.pvsAllegroDebugPrompt) ) {
			String question = "";
			// numberCameSeparate was added because sometimes the index of the option (e.g. " 0: ") would come in a separate line than its description.
			String numberCameSeparate = null;
			Pattern pattern = Pattern.compile(PVSConstants.pvsErrorMessageOptionIndex);
			ArrayList<String> choices = new ArrayList<String>();
			for (String line: previousLines) {
				if ( line.matches(PVSConstants.pvsErrorMessageOptionIndex) ) {
					numberCameSeparate = line;
					continue;
				}
				Matcher m = pattern.matcher(line);
				if ( m.find() ) {
					if ( m.start() == 0 ) {
						choices.add(line);
					}
				} else {
					if ( numberCameSeparate != null ) {
						String apnds = choices.get(choices.size()-1);
						choices.remove(choices.size()-1);
						choices.add(numberCameSeparate + apnds);
						numberCameSeparate = null;
					} else {
						question = question + line + PVSConstants.NL;
					}
				}
			}
			int respond = EclipseGuiUtil.askUserMultipleChoice(question, choices);
			if ( respond > -1 ) {
				PVSExecutionManager.writeToPVS(PVSConstants._CONTINUE + respond);
			}
		}
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
