package com.sri.csl.pvs.plugin;

import java.util.regex.Pattern;

import junit.framework.TestCase;

import org.junit.Test;

import com.sri.csl.pvs.PVSConstants;
import com.sri.csl.pvs.PVSPromptProcessor;

public class PVSTest extends TestCase {

	protected static void tell(String s) {
		System.out.println(s);
	}
		
	@Test
	public void testAllegroPrompt() {
		Pattern pattern = PVSPromptProcessor.getPVSPromptPattern(PVSConstants.ALLEGRO);

		tell("Testing ALLEGRO regex:  " + pattern.pattern());
		
		String[] acceptAsPrompt = new String[] {
				"Please enter: ",
				"(Y or N) ",
				"(Yes or No) ",
				"yices > ",
				"<PVSio> ",
				"sjdgfhjsdg> ",
				"pvs(2): ",
				"[1] pvs(34): ",
		};

		String[] rejectAsPrompt = new String[] {
				"pvs",
				"Allegro CL Enterprise Edition",
				"Context file /Applications/eclipse/Eclipse.app/Contents/MacOS/.pvscontext written",
		};
		
		for (String str: acceptAsPrompt) {
			tell("Prompt Testing: " + str);
			boolean matched = pattern.matcher(str).matches();
			assertTrue(str, matched);
		}
	
		for (String str: rejectAsPrompt) {
			tell("Non-Prompt Testing: " + str);
			boolean matched = pattern.matcher(str).matches();
			assertFalse(str, matched);
		}
	}
	
	@Test
	public void testCMUPrompt() {
		Pattern pattern = PVSPromptProcessor.getPVSPromptPattern(PVSConstants.CMU);

		tell("Testing CMU regex:  " + pattern.pattern());
		
		String[] acceptAsPrompt = new String[] {
				"Please enter",
				"pvs(2):",
		};

		String[] rejectAsPrompt = new String[] {
				"pvs",
				"Allegro CL Enterprise Edition",
				"Context file /Applications/eclipse/Eclipse.app/Contents/MacOS/.pvscontext written",
		};
		
		for (String str: acceptAsPrompt) {
			tell("Prompt Testing: " + str);
			boolean matched = pattern.matcher(str).matches();
			assertTrue(str, matched);
		}
	
		for (String str: rejectAsPrompt) {
			tell("Non-Prompt Testing: " + str);
			boolean matched = pattern.matcher(str).matches();
			assertFalse(str, matched);
		}
	}

}
