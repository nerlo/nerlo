package org.ister.graphdb.executor;

import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.graphdb.Transaction;

public class DelVertexExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return msg.has("id");
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		if (deleteNode((Long) msg.get("id"))) {
			Map<String, Object> map = new HashMap<String, Object>(1);
			map.put("result", "ok");
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		} else {
			throw new ExecutorException("could_not_delete");
		}
	}

	@Override
	protected String getId() {
		return "del_vertex";
	}
	
	private boolean deleteNode(Long id) {
		boolean success = false;
		Transaction tx = this.db.beginTx();
		try {
			org.neo4j.graphdb.Node node = this.db.getNodeById(id);
			node.delete();
			success = true;
			tx.success();
		} catch (Exception e) {
			log.error("could not delete node " + id.toString() + ": " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		return success;
	}

}
