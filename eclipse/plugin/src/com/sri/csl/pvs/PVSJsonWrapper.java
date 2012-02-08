package com.sri.csl.pvs;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class PVSJsonWrapper implements PVSExecutionManager.PVSRespondListener {
	protected static String ID = "id";
	protected static String COMMAND = "command";
	protected static String PARAMETERS = "parameters";
	protected static String RESULT = "result";
	protected static String ERROR = "error";
	protected static String RAWCOMMAND = "rawcommand";
	
	
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
	
	synchronized public void addToJSONQueue(JSONObject obj) {
		responds.add(obj);
	}
	
	public Object sendRawCommand(String message) throws PVSException {
		HashMap<String, String> map = new HashMap<String, String>();
		String id = createID();
		map.put(ID, id);
		
		map.put(RAWCOMMAND, message);
		
		JSONObject obj = new JSONObject(map);
		
		return sendJSON(id, obj);
	}

	public Object sendCommand(String command, Object... parameters) throws PVSException {
		HashMap<String, Object> map = new HashMap<String, Object>();
		String id = createID();
		map.put(ID, id);
		
		map.put(COMMAND, command);
		
		try {
			JSONArray jParams = new JSONArray(parameters);
			map.put(PARAMETERS, jParams);
		} catch (JSONException e) {
			throw new PVSException(e.getMessage());
		}
		
		JSONObject obj = new JSONObject(map);
		
		return sendJSON(id, obj);
	}

	private Object sendJSON(String id, JSONObject obj) throws PVSException {
		String  pvsJSON = "(pvs-json \"" + obj.toString().replace("\"", "\\\"") + "\")";
		PVSExecutionManager.writeToPVS(pvsJSON);
		int MAX = 30;
		for (int i=0; i<50*MAX; i++) {
			synchronized( responds ) {
				for (JSONObject respond: responds) {
					try {
						String rid = respond.getString(ID);
						if ( id.equals(rid) ) {
							responds.remove(respond);
							if ( respond.has(ERROR) ) {
								throw new PVSException(respond.getString(ERROR));
							} else {
								return respond.get(RESULT);
							}
						}
					} catch (Exception e) {
						throw new PVSException(e.getMessage());
					}
				}
			}
			try {
				Thread.sleep(20);
			} catch (InterruptedException e) {
				throw new PVSException(e.getMessage());
			}
		}
		throw new PVSException("No response from PVS after " + MAX + " seconds");
	}
	
	@Override
	public void onMessageReceived(String message) {
	}

	@Override
	public void onMessageReceived(JSONObject message) {
		responds.add(message);
		
	}
}
