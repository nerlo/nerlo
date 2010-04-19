package org.ister.graphdb.executor;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.graphdb.Transaction;
import org.neo4j.index.IndexHits;

public class IndexExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return ((msg.has("op") && msg.has("id") && msg.has("key") && msg.has("value")));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		if (msg.get("op").equals("lookup")) {
			Map<String, Object> map = Collections.singletonMap("result", lookup((String)msg.get("key"), (String)msg.get("value")));
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		} else if (msg.get("op").equals("add") || msg.get("op").equals("del")) {
			op((Long)msg.get("id"), (String)msg.get("key"), (String)msg.get("value"), (String)msg.get("op"));
			Map<String, Object> map = Collections.singletonMap("result", (Object)"ok");
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		} else {
			throw new RuntimeException("unknown operation: " + msg.get("op"));
		}
	}

	@Override
	protected String getId() {
		return "index";
	}
	
	private Object lookup(String key, String value) throws ExecutorException {
		long id = -1L;
		Transaction tx = this.db.beginTx();
		try {
			IndexHits<org.neo4j.graphdb.Node> hits = this.index.getNodes(key, value);
			if (hits.size() < 1) {
				throw new ExecutorException("no_vertex_found");
			} else if (hits.size() > 1) {
				throw new ExecutorException("multiple_vertices_found");
			}
			id = hits.next().getId();
			tx.success();
		} catch (ExecutorException e) {
			throw e;
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
		return new Long(id);
	}
	
	private void op(Long id, String Key, String Value, String op) throws ExecutorException {
		Transaction tx = this.db.beginTx();
		try {
			org.neo4j.graphdb.Node node = this.db.getNodeById(id.longValue());
			if (op.equals("add")) {
				this.index.index(node, Key, Value);
			} else if (op.equals("del")) {
				this.index.removeIndex(node, Key, Value);
			}
			tx.success();
		} catch (NotFoundException e) {
			tx.failure();
			throw new ExecutorException("vertex_not_found");
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
	}


}
