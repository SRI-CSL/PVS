package com.sri.csl.pvs.plugin;

import java.util.regex.Pattern;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.Test;

import com.sri.csl.pvs.PVSExecutionManager;

public class PVSTest extends TestCase {

	protected static void tell(String s) {
		System.out.println(s);
	}
	
	@Test	
	public void testPrompt() {
		String promptRegex = PVSExecutionManager.getPVSPromptRegex(1);
		Pattern pvsPromptPattern = Pattern.compile(promptRegex);
		String[] acceptAsPrompt = new String[] {
				"pvs(2):",
				"Please enter",
		};

		String[] rejectAsPrompt = new String[] {
				"pvs",
				"Allegro CL Enterprise Edition",
				"Context file /Applications/eclipse/Eclipse.app/Contents/MacOS/.pvscontext written",
		};
		
		for (String str: acceptAsPrompt) {
			tell("Prompt Testing: " + str);
			boolean matched = pvsPromptPattern.matcher(str).find();
			assertTrue(matched);
		}
	
		for (String str: rejectAsPrompt) {
			tell("Non-Prompt Testing: " + str);
			boolean matched = pvsPromptPattern.matcher(str).find();
			assertFalse(matched);
		}
	}
	
	

}
