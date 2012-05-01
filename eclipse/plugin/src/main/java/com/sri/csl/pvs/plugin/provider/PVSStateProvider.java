package com.sri.csl.pvs.plugin.provider;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.ISources;

import com.sri.csl.pvs.PVSConstants;
import com.sri.csl.pvs.PVSExecutionManager;

public class PVSStateProvider extends AbstractSourceProvider implements PVSStateChangeListener {

	@Override
	public void dispose() {
	}
	
	public PVSStateProvider() {
		super();
		PVSExecutionManager.INST().addListener(this);
	}

	@Override
	public Map<String, String> getCurrentState() {
		Map<String, String> map = new HashMap<String, String>(1);
		String value = PVSExecutionManager.INST().isPVSRunning() ? PVSConstants.TRUE : PVSConstants.FALSE;
		map.put(PVSConstants.PVSRUNNING, value);
		return map;
	}

	@Override
	public String[] getProvidedSourceNames() {
		return new String[] {PVSConstants.PVSRUNNING};
	}

	@Override
	public void sourceChanged(String key, String value) {
		fireSourceChanged(ISources.WORKBENCH, key, value);
	}

}
