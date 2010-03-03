package org.ister.ej;

import org.apache.log4j.Logger;

public class SimpleMsgHandler extends AbstractMsgHandler {

	private Logger log = Main.getLogger();
	
	@Override
	public void handle(Msg msg) {
        log.warn("unhandled message from " 
        		 + msg.getFrom().toString() 
      		     + ": " 
      		     + msg.getMsg().toString());
	}

	@Override
	public void shutdown() {}

}
