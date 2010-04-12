package org.ister.graphdb.executor;

import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Transaction;

public class DelPropertyExecutor extends AbstractGraphdbMsgExecutor {

	private final static String VERTEX = "vertex";
	private final static String EDGE = "edge";
	
	@Override
	protected boolean checkMsg(Msg msg) {
		return (msg.has("id") && msg.has("type") && msg.has("key"));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		if (del((String)msg.get("type"), (Long) msg.get("id"), (String)msg.get("key"))) {
			Map<String, Object> map = new HashMap<String, Object>(1);
			map.put("result", "ok");
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		} else {
			throw new ExecutorException("could_not_del_property");
		}
	}

	@Override
	protected String getId() {
		return "del_property";
	}
	
	private boolean del(String type, Long id, String name) {
		boolean success = false;
		Transaction tx = this.db.beginTx();
		try {
			PropertyContainer entity = null;
			if (type.equals(VERTEX)) {
				entity = this.db.getNodeById(id);
			} else if (type.equals(EDGE)) {
				entity = this.db.getRelationshipById(id);
			} else {
				throw new Exception("unknown object type: " + type);
			}
			entity.removeProperty(name);
			success = true;
			tx.success();
		} catch (Exception e) {
			log.error("could not delete property for " + type + "=" + id.toString() + ": " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		return success;
	}

}
