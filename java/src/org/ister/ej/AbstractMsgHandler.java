package org.ister.ej;


/**
 * 
 * @author ingo
 *
 */
public abstract class AbstractMsgHandler implements MsgHandler {

	private Node node = null;
	
	public abstract void handle(Msg msg);
	public abstract void shutdown();
	
	public final void setNode(Node node) {
		this.node = node;
	}
	
	protected final Node getNode() {
		return this.node;
	}
	
}
