package com.sri.csl.pvs.plugin.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;

public class PVSLaunchConfigurationDelegate implements ILaunchConfigurationDelegate, IDebugEventSetListener {

	@Override
	public void handleDebugEvents(DebugEvent[] arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void launch(ILaunchConfiguration configuration, String mode, ILaunch launch, IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub
		
	}

}
