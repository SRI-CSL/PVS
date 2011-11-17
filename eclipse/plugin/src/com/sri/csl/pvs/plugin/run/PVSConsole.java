package com.sri.csl.pvs.plugin.run;

import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IOConsole;


public class PVSConsole extends IOConsole {
	private static String name = "PVS Console";
	
	private PVSConsole() {
		super(name, null, null, true);
	}
	
	public static PVSConsole getConsole() {
		ConsolePlugin plugin = ConsolePlugin.getDefault();
		IConsoleManager conMan = plugin.getConsoleManager();
		for (IConsole c: conMan.getConsoles()) {
			if (  name.equals(c.getName()) ) {
				return (PVSConsole)c;
			}
		}
		PVSConsole console = new PVSConsole();
		conMan.addConsoles(new IConsole[]{console});
		conMan.showConsoleView(console);	
		return console;
	}

	
}
