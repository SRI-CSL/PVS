package com.sri.csl.pvs;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;

public class PVSCommandManager {
	public static Object handleCommand(String name, String ...arguments) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException {
		System.out.println("PVS Command to run: " + name);
		Method[] methods = PVSCommands.class.getDeclaredMethods();
		for (Method method: methods) {
			if ( method.getName().equals(name) ) {
				return method.invoke(null, Arrays.asList(arguments));
			}
		}
		
		return null;
	}

}
