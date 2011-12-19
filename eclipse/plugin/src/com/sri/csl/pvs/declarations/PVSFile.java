package com.sri.csl.pvs.declarations;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;

public class PVSFile {
	protected boolean processed;
	protected String content = null;
	protected String filename;
	protected ArrayList<PVSTheory> theories = new ArrayList<PVSTheory>();
	
	public PVSFile(String fn) {
		processed = false;
		filename = fn;
		load();
	}
	
	public String getFilename() {
		return filename;
	}
	
	public void load() {
		BufferedReader reader;
		try {
			reader = new BufferedReader( new FileReader(filename));
		    String line  = null;
		    StringBuilder stringBuilder = new StringBuilder();
		    String ls = System.getProperty("line.separator");
		    while( ( line = reader.readLine() ) != null ) {
		        stringBuilder.append( line );
		        stringBuilder.append( ls );
		    }
		    content = stringBuilder.toString();
		} catch (Exception e) {
			e.printStackTrace();
		}		
	}

	public void process() {
		if ( processed ) return;
		theories.clear();		
		// TODO: The file content is in 'content'. 
		// TODO: Find Theories and add them to the list.
		//.......
	}
	
	public boolean isProcessed() {
		return processed;
	}
	
}
