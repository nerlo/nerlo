package org.ister.graphdb;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.log4j.Logger;

import org.ister.ej.AbstractMsgHandler;
import org.ister.ej.Main;
import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.ej.Node;
import org.ister.ej.ConcurrencyUtil;
import org.ister.graphdb.executor.*;
import org.ister.nerlo.Fiber;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
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
	private final ExecutorService yielder = Executors.newSingleThreadExecutor();
	
	private final ExecutorService exec = Executors.newCachedThreadPool();
	private final CompletionService<Msg> service = new ExecutorCompletionService<Msg>(this.exec);
	
	public void init(Node node) {
		super.init(node);
		
		this.path = pwd + "/" + Main.getProperty("graphdb.db.path", "db");
		
		this.map.put("add_vertex", AddVertexExecutor.class);
		this.map.put("vertex_get_edges", VertexGetEdgesExecutor.class);
		this.map.put("del_vertex", DelVertexExecutor.class);
		this.map.put("add_edge", AddEdgeExecutor.class);
		this.map.put("del_edge", DelEdgeExecutor.class);
		this.map.put("set_property", SetPropertyExecutor.class);
		this.map.put("del_property", DelPropertyExecutor.class);
		this.map.put("get_property", GetPropertyExecutor.class);
		this.map.put("get_properties", GetPropertiesExecutor.class);
		this.map.put("info", InfoExecutor.class);
		this.map.put("index", IndexExecutor.class);
		
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

        final Node nodeRef = node;
    	yielder.submit(new Runnable() {
            public void run(){
            	log.debug("yielder entering run loop");
                while (true) {
                   try {
                   	   Future<Msg> fu = service.take();
                   	   nodeRef.sendPeer(fu.get());
                   } catch(ExecutionException e) {
                       log.error("Exception: \n" + e.toString());
                       ConcurrencyUtil.peelException(e.getCause());
                   } catch(InterruptedException e) {
                   	   log.error("Exception: \n" + e.toString());
                   	   Thread.currentThread().interrupt();
                   }
                }
            }
        });
    	log.debug("yielder has been set");
        
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
    				cache.clear();
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
    			if (this.db == null || this.index == null) {
    				node.sendPeer(errorAnswer(msg, "no_db"));
    				return;
    			}
    			String id = (String) msg.get("call");
    			AbstractGraphdbMsgExecutor ex = getExecutor(id);
    			if (ex == null) {
    				Msg answer = errorAnswer(msg, "no_executor");
    				node.sendPeer(answer);
    			} else {
    				ex.setMsg(msg);
    				this.service.submit(ex);
    			}
    		    return;
    		} 
    	}
    	
		logUnhandledMsg(log, msg);
	}

	@Override
	public void shutdown() {
		exec.shutdown();
		dbShutdown();
		yielder.shutdown();
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
		if (this.db == null) {
			return runDbInit(path);
		}
		return true;
	}
	
	private boolean runDbInit(String path) {
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
			this.index = null;
		}
		if (this.db instanceof GraphDatabaseService) {
			this.db.shutdown();
			this.db = null;
		}
		log.info("database shutdown completed");
	}

}
