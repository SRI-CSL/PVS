package com.sri.csl.pvs.plugin.provider;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.ISources;

import com.sri.csl.pvs.PVSExecutionManager;

public class PVSStateProvider extends AbstractSourceProvider {
	public final static String PVSRUNNING = "com.sri.csl.pvs.plugin.provider.isPVSRunning";
	public final static String TRUE = "TRUE";
	public final static String FALSE = "FALSE";

	@Override
	public void dispose() {
	}

	@Override
	public Map<String, String> getCurrentState() {
		Map<String, String> map = new HashMap<String, String>(1);
		String value = PVSExecutionManager.isPVSRunning() ? TRUE : FALSE;
		map.put(PVSRUNNING, value);
		return map;
	}

	@Override
	public String[] getProvidedSourceNames() {
		return new String[] { PVSRUNNING };
	}
	
	public void updateState(String stateName, String value) {
		fireSourceChanged(ISources.WORKBENCH, stateName, value);
	}
}
