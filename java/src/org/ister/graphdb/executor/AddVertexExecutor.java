package org.ister.graphdb.executor;

import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.Transaction;


public class AddVertexExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return true;
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		Long id = createNode();
		if (id == null) {
			throw new ExecutorException("could_not_create");
		} else {
			Map<String, Object> map = new HashMap<String, Object>(1);
			map.put("result", id);
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		}
	}

	@Override
	protected String getId() {
		return "add_vertex";
	}
	
	public Long createNode() {
		Long id = null;
		Transaction tx = this.db.beginTx();
		try {
			org.neo4j.graphdb.Node node = this.db.createNode();
			id = Long.valueOf(node.getId());
			tx.success();
		} catch (Exception e) {
			log.error("could not create node: " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		return id;
	}

}
