package org.ister.graphdb.executor;

import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Transaction;

public class GetPropertyExecutor extends AbstractGraphdbMsgExecutor {

	private final static String VERTEX = "vertex";
	private final static String EDGE = "edge";
	
	@Override
	protected boolean checkMsg(Msg msg) {
		return (msg.has("id") && msg.has("type") && msg.has("key"));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		Object property = get((String)msg.get("type"), (Long) msg.get("id"), (String)msg.get("key"));
		if (property == null) {
			throw new ExecutorException("could_not_get_property");
		}
		Map<String, Object> map = new HashMap<String, Object>(1);
		map.put("result", property);
		return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
	}

	@Override
	protected String getId() {
		return "get_property";
	}
	
	private Object get(String type, Long id, String name) {
		Object property = null;
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
			property = entity.getProperty(name);
			tx.success();
		} catch (Exception e) {
			log.error("could not get property for " + type + "=" + id.toString() + ": " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		return property;
	}

}
