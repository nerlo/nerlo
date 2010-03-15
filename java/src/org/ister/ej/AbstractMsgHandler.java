package org.ister.ej;

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
	
	protected static void logUnhandledMsg(Logger log, Msg msg) {
		log.warn("unhandled message from " 
				+ msg.getFrom().toString() 
				+ ": " + msg.getMsg().toString()
				+ " -- transformed paylod: "
				+ msg.getMap().toString());
	}
	
}
