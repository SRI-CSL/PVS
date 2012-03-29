package com.sri.csl.pvs.plugin;

import junit.framework.TestCase;

import org.junit.Test;

import com.sri.csl.pvs.PVSConstants;

public class PVSTest extends TestCase {

	protected static void tell(String s) {
		System.out.println(s);
	}
		
	@Test
	public void testAllegroPrompt() {
		
		String promptRegex = PVSConstants.pvsAllegroPrompt;

		tell("Testing ALLEGRO regex:  " + promptRegex);
		
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
			boolean matched = str.matches(promptRegex);
			assertTrue(str, matched);
		}
	
		for (String str: rejectAsPrompt) {
			tell("Non-Prompt Testing: " + str);
			boolean matched = str.matches(promptRegex);
			assertFalse(str, matched);
		}
	}
	
	@Test
	public void testCMUPrompt() {
		
		String promptRegex = PVSConstants.pvsCmuPrompt;
		tell("Testing CMU regex:  " + promptRegex);
		
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
			boolean matched = str.matches(promptRegex);
			assertTrue(str, matched);
		}
	
		for (String str: rejectAsPrompt) {
			tell("Non-Prompt Testing: " + str);
			boolean matched = str.matches(promptRegex);
			assertFalse(str, matched);
		}
	}

}
