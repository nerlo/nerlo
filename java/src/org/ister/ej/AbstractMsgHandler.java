package org.ister.ej;

import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;


/**
 * 
 * @author ingo
 *
 */
public abstract class AbstractMsgHandler implements MsgHandler {

	private Node node = null;
	
	public abstract void handle(Msg msg);
	public abstract void shutdown();
	
	public void init(Node node) {
		this.node = node;
	}
	
	protected final Node getNode() {
		return this.node;
	}
	
	protected Msg errorAnswer(Msg msg, String reason) {
		Map<String, Object> map = new HashMap<String, Object>(2);
	    map.put("reason", reason);
	    return Msg.answer(node.getSelf(), MsgTag.ERROR, map, msg);
	}
	
	protected static void logUnhandledMsg(Logger log, Msg msg) {
		log.warn("unhandled message from " 
				+ msg.getFrom().toString() 
				+ ": " + msg.getMsg().toString()
				+ " -- transformed paylod: "
				+ msg.getMap().toString());
	}
	
}
