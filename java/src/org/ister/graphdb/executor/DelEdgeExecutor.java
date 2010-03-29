package org.ister.graphdb.executor;

import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

public class DelEdgeExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return msg.has("id");
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		if (deleteEdge((Long) msg.get("id"))) {
			Map<String, Object> map = new HashMap<String, Object>(2);
			map.put("result", "ok");
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		} else {
			throw new ExecutorException("could_not_delete");
		}		
	}

	@Override
	protected String getId() {
		return "del_edge";
	}
	
	private boolean deleteEdge(Long id) {
		boolean success = false;
		Transaction tx = this.db.beginTx();
		try {
			Relationship edge = this.db.getRelationshipById(id) ;
			edge.delete();
			success = true;
			tx.success();
		} catch (Exception e) {
			log.error("could not delete edge " + id.toString() + ": " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		return success;
	}

}
