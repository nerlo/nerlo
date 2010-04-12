package org.ister.graphdb;

import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;

import org.ister.ej.AbstractMsgHandler;
import org.ister.ej.Main;
import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.ej.Node;
import org.ister.graphdb.executor.*;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.index.IndexService;
import org.neo4j.index.lucene.LuceneIndexService;
import org.neo4j.kernel.EmbeddedGraphDatabase;

public class DbMsgHandler extends AbstractMsgHandler {

	private final Logger log = Main.getLogger();
	private final String pwd = System.getProperty("user.dir");
	@SuppressWarnings("unchecked")
	private final HashMap<String, Class> map = new HashMap<String, Class>();
	private final HashMap<String, AbstractGraphdbMsgExecutor> cache = new HashMap<String, AbstractGraphdbMsgExecutor>();
	
	private String path = null;
	private GraphDatabaseService db = null;
	private IndexService index = null;
	
	public void init(Node node) {
		super.init(node);
		
		this.path = pwd + "/" + Main.getProperty("graphdb.db.path", "db");
		
		this.map.put("add_vertex", AddVertexExecutor.class);
		this.map.put("vertex_get_edges", VertexGetEdgesExecutor.class);
		this.map.put("del_vertex", DelVertexExecutor.class);
		this.map.put("add_edge", AddEdgeExecutor.class);
		this.map.put("del_edge", DelEdgeExecutor.class);
		//this.map.put("set_property", AddPropertyExecutor.class);
		
        // almost always shutdown database
		final DbMsgHandler hdl = this;
        Runtime.getRuntime().addShutdownHook(
        	new Thread(new Runnable() {
	            public void run(){
	                hdl.dbShutdown();
	            }
            })
        );
        log.debug("shutdown hook has been set");
        
        log.info("initialized: " + this.getClass().toString());
	}
	
	@Override
	public void handle(Msg msg) {
		Node node = getNode();
		MsgTag tag = msg.getTag();
		
    	if (tag.equals(MsgTag.CALL)) {
    		if (msg.match("call", "init")) {
    			Msg answer = null;
    			if (dbInit(this.path)) {
    				Map<String, Object> map = new HashMap<String, Object>(2);
	    		    map.put("result", true);
	    		    answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    			} else {
    				answer = errorAnswer(msg, "no_db");
    			}
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "stop")) {
    			dbShutdown();
    			Map<String, Object> map = new HashMap<String, Object>(2);
    		    map.put("result", "ok");
    		    Msg answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.match("call", "has_db")) {
    			Map<String, Object> map = new HashMap<String, Object>(2);
    		    map.put("result", (this.db != null));
    		    Msg answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
    		    node.sendPeer(answer);
    		    return;
    		} else if (msg.has("call")) {
    			Msg answer = null;
    			String id = (String) msg.get("call");
    			AbstractGraphdbMsgExecutor ex = getExecutor(id);
    			if (ex == null) {
    				answer = errorAnswer(msg, "no_executor");
    			} else {
        			answer = ex.exec(msg);
    			}
    			node.sendPeer(answer);
    		    return;
    		} 
    	}
    	
		logUnhandledMsg(log, msg);
	}

	@Override
	public void shutdown() {
		dbShutdown();
	}
	
	
	private AbstractGraphdbMsgExecutor getExecutor(String id) {
		if (!cache.containsKey(id)) {
			if (!map.containsKey(id)) {
				log.error("no executor for '" + id);
				return null;
			}
			try {
				@SuppressWarnings("unchecked")
				Class clazz = map.get(id);
				AbstractGraphdbMsgExecutor ex = (AbstractGraphdbMsgExecutor) clazz.newInstance();
				ex.init(getNode(), this.db, this.index);
				cache.put(id, ex);
			} catch (InstantiationException e) {
				log.error("failed to create executor for '" + id + "': " + e.toString());
				return null;
			} catch (IllegalAccessException e) {
				log.error("failed to create executor for '" + id + "': " + e.toString());
				return null;
			}
		}
		return cache.get(id);
	}
	
	private boolean dbInit(String path) {
		if (this.db instanceof GraphDatabaseService) {
			return true;
		}
		try {
			this.db = new EmbeddedGraphDatabase(path);
			this.index = new LuceneIndexService(this.db);
			log.info("graph database initialized: " + path);
		} catch (Exception e) {
			log.error("initialization of database failed: " + e.toString());
			return false;
		}
		return true;
	}
	
	private void dbShutdown() {
		if (this.index instanceof IndexService) {
			this.index.shutdown();
		}
		if (this.db instanceof GraphDatabaseService) {
			this.db.shutdown();
		}
	}

}
