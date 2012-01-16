package com.sri.csl.pvs;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;

import org.json.JSONObject;

public class PVSJsonWrapper implements PVSExecutionManager.PVSRespondListener {
	protected static String ID = "id";
	protected static String COMMAND = "command";
	protected static String PARAMETERS = "parameters";
	protected static String RESPOND = "respond";
	protected static String ERROR = "error";
	protected static String RAWCOMMAND = "rawcommand";
	protected static String RAWRESPOND = "rawrespond";
	
	
	protected Calendar cal = Calendar.getInstance();
	protected ArrayList<JSONObject> responds = new ArrayList<JSONObject>();
	
	private static PVSJsonWrapper instance = null;
	
	private PVSJsonWrapper() {
		super();
	}
	
	public static void init() {
		if ( instance == null ) {
			instance = new PVSJsonWrapper();
		}
	}
	
	public static PVSJsonWrapper INST() {
		return instance;
	}
	
	
	protected String createID() {
		return "message_id_" + cal.getTimeInMillis();
	}
	
	public String sendRawMessage(String message) throws PVSException {
		HashMap<String, String> map = new HashMap<String, String>();
		String id = createID();
		map.put(ID, id);
		
		map.put(RAWCOMMAND, message);
		
		JSONObject obj = new JSONObject(map);
		
		String jsonString = obj.toString();
		
		PVSExecutionManager.writeToPVS(jsonString);
		while ( true ) {
			for (JSONObject respond: responds) {
				try {
					String rid = respond.getString(ID);
					if ( id.equals(rid) ) {
						if ( respond.has(ERROR) ) {
							throw new PVSException(respond.getString(ERROR));
						} else {
							return respond.getString(RAWRESPOND);
						}
					}
				} catch (Exception e) {
					throw new PVSException(e.getMessage());
				}
			}
			try {
				Thread.sleep(20);
			} catch (InterruptedException e) {
				throw new PVSException(e.getMessage());
			}
		}
	}

	@Override
	public void onMessageReceived(String message) {
		// Do Nothing
		
	}

	@Override
	public void onMessageReceived(JSONObject message) {
		responds.add(message);
		
	}
}
