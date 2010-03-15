package org.ister.graphdb;

import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;
import org.ister.ej.AbstractMsgHandler;
import org.ister.ej.Main;
import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.ej.Node;

public class DbMsgHandler extends AbstractMsgHandler {

	private final Logger log = Main.getLogger();
	private Db db = null;
	
	public void init(Node node) {
		super.init(node);
		this.db = new Db(Main.getProperty("graphdb.db.path", "db"));
		log.info("initialized: " + this.getClass().toString());
	}
	
	@Override
	public void handle(Msg msg) {
		Node node = getNode();
		MsgTag tag = msg.getTag();
		
    	if (tag.equals(MsgTag.CALL)) {
    		if (msg.match("call", "init")) {
    			boolean init = this.db.init();
    			String aswtag = init ? MsgTag.OK : MsgTag.ERROR;
    			Map<String, Object> map = new HashMap<String, Object>(2);
    		    map.put("result", init);
    		    Msg answer = Msg.answer(node.getSelf(), aswtag, map, msg);
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "has_db")) {
    			Map<String, Object> map = new HashMap<String, Object>(2);
    		    map.put("result", this.db.hasDb());
    		    Msg answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    		    node.sendPeer(answer);
    		    return;
    		}
    	}
    	
		logUnhandledMsg(log, msg);
	}

	@Override
	public void shutdown() {
		this.db.shutdown();
	}

}
