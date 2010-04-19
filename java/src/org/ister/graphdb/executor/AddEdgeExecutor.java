package org.ister.graphdb.executor;

import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;
import org.ister.ej.Main;
import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.ej.Node;
import org.ister.graphdb.Relations;
import org.ister.nerlo.ExecutorException;
import org.ister.nerlo.AbstractMsgExecutor;
import org.neo4j.graphdb.DynamicRelationshipType;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.index.IndexService;

public class AddEdgeExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return ((msg.has("a") && msg.has("b") && msg.has("type")));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		Long id = addEdge(
					(Long)    msg.get("a"),
					(Long)    msg.get("b"),
					(String)  msg.get("type"));
		if (id == null) {
			throw new ExecutorException("could_not_create_edge");
		} else {
			Map<String, Object> map = new HashMap<String, Object>(1);
			map.put("result", id);
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		}		
	}

	@Override
	protected String getId() {
		return "add_edge";
	}
	
	private Long addEdge(Long a, Long b, String type) {
		Long id = null;
		Transaction tx = this.db.beginTx();
		try {
			org.neo4j.graphdb.Node na = this.db.getNodeById(a);
			org.neo4j.graphdb.Node nb = this.db.getNodeById(b);
			RelationshipType rt = null;
			try {
				rt = Relations.valueOf(type.toUpperCase());
			} catch (IllegalArgumentException e) {
				rt = DynamicRelationshipType.withName(type);
			}
			Relationship edge = na.createRelationshipTo(nb, rt);
			id = Long.valueOf(edge.getId());
			tx.success();
		} catch (Exception e) {
			log.error("could not create edge: " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		return id;
	}

}
