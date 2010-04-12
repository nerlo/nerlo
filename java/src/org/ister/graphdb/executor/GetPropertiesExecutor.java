package org.ister.graphdb.executor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Transaction;

public class GetPropertiesExecutor extends AbstractGraphdbMsgExecutor {

	private final static String VERTEX = "vertex";
	private final static String EDGE = "edge";
	
	@Override
	protected boolean checkMsg(Msg msg) {
		return (msg.has("id") && msg.has("type"));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		Object property = get((String)msg.get("type"), (Long) msg.get("id"));
		if (property == null) {
			throw new ExecutorException("could_not_get_properties");
		}
		Map<String, Object> map = new HashMap<String, Object>(1);
		map.put("result", property);
		return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
	}

	@Override
	protected String getId() {
		return "get_property";
	}
	
	private List<Map<Integer,Object>> get(String type, Long id) throws ExecutorException {
		ArrayList<Map<Integer,Object>> properties = new ArrayList<Map<Integer,Object>>();
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
			for (String k : entity.getPropertyKeys()) {
				Map<Integer,Object> tuple = new TreeMap<Integer,Object>();
				tuple.put(1, k);
				tuple.put(2, entity.getProperty(k));
				properties.add(tuple);
			}
			tx.success();
		} catch (Exception e) {
			log.error("could not get property for " + type + "=" + id.toString() + ": " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		return properties;
	}

}
