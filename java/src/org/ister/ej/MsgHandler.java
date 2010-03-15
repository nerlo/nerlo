package org.ister.ej;


/**
 * 
 * @author ingo
 *
 */
public interface MsgHandler {
	
	public void handle(Msg msg);
	public void init(Node node);
	public void shutdown();
	
}
