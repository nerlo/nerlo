package org.ister.graphdb.executor;

import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Transaction;

public class SetPropertyExecutor extends AbstractGraphdbMsgExecutor {

	private final static String VERTEX = "vertex";
	private final static String EDGE = "edge";
	
	@Override
	protected boolean checkMsg(Msg msg) {
		return (msg.has("id") && msg.has("type") && msg.has("key") && msg.has("value"));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		if (set((String)msg.get("type"), (Long) msg.get("id"), (String)msg.get("key"), msg.get("value"))) {
			Map<String, Object> map = new HashMap<String, Object>(1);
			map.put("result", "ok");
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		} else {
			throw new ExecutorException("could_not_set_property");
		}
	}

	@Override
	protected String getId() {
		return "set_property";
	}
	
	private boolean set(String type, Long id, String name, Object value) {
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
			entity.setProperty(name, value);
			success = true;
			tx.success();
		} catch (Exception e) {
			log.error("could not set property for " + type + "=" + id.toString() + ": " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		return success;
	}

}
