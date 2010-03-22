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
		
		// TODO Can we do this in O(1) instead O(n)?
		// We could have some HashMap containing call values
		// as keys and Runnables as values (lots of...).
		// Create these Runnables lazily but cache once instantiated.
    	if (tag.equals(MsgTag.CALL)) {
    		if (msg.match("call", "init")) {
    			Msg answer = null;
    			if (this.db.init()) {
    				Map<String, Object> map = new HashMap<String, Object>(2);
	    		    map.put("result", true);
	    		    answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    			} else {
    				answer = errorAnswer(msg, "no_db");
    			}
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "stop")) {
    			this.db.shutdown();
    			Map<String, Object> map = new HashMap<String, Object>(2);
    		    map.put("result", "ok");
    		    Msg answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "has_db")) {
    			Map<String, Object> map = new HashMap<String, Object>(2);
    		    map.put("result", this.db.hasDb());
    		    Msg answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "add_vertex")) {
    			Msg answer = null;
    			Long id = this.db.createNode();
    			if (id == null) {
    				answer = errorAnswer(msg, "could_not_create");
    			} else {
    				Map<String, Object> map = new HashMap<String, Object>(2);
    				map.put("result", id);
    				answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    			}
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "del_vertex")) {
    			Msg answer = null;
    			if (!msg.has("id")) {
    				answer = errorAnswer(msg, "no_id_submitted");
    			} else {
	    			if (this.db.deleteNode((Long) msg.get("id"))) {
	    				Map<String, Object> map = new HashMap<String, Object>(2);
	    				map.put("result", "ok");
	    				answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
	    			} else {
	    				answer = errorAnswer(msg, "could_not_delete");
	    			}
    			}
    			node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "add_edge")) {
    			Msg answer = null;
    			if (!(msg.has("a") && msg.has("b") && msg.has("type") && msg.has("dir"))) {
    				answer = errorAnswer(msg, "data_missing");
    			} else {
    				Long id = this.db.addEdge(
    							(Long)    msg.get("a"),
    							(Long)    msg.get("b"),
    							(String)  msg.get("type"),
    							(Boolean) msg.get("dir"));
    				if (id == null) {
    					answer = errorAnswer(msg, "could_not_create");
    				} else {
    					Map<String, Object> map = new HashMap<String, Object>(2);
    					map.put("result", id);
    					answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    				}
    			}
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "del_edge")) {
    			Msg answer = null;
    			if (!msg.has("id")) {
    				answer = errorAnswer(msg, "data_missing");
    			} else {
    				if (this.db.deleteEdge((Long) msg.get("id"))) {
    					Map<String, Object> map = new HashMap<String, Object>(2);
    					map.put("result", "ok");
    					answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    				} else {
    					answer = errorAnswer(msg, "could_not_delete");
    				}
    			}
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
