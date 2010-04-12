package org.ister.graphdb.executor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.DynamicRelationshipType;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

public class VertexGetEdgesExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return msg.has("id");
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		ArrayList<TreeMap<Integer,Object>> rs = getRelations((Long) msg.get("id"));
		if (rs == null) {
			throw new ExecutorException("could_not_read_relations");
		} else {
			Map<String, Object> map = new HashMap<String, Object>(2);
			map.put("result", rs);
			return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
		}
	}

	@Override
	protected String getId() {
		return "vertex_get_edges";
	}
	
	private ArrayList<TreeMap<Integer,Object>> getRelations(Long id) {
		ArrayList<TreeMap<Integer,Object>> list = null;
		
		org.neo4j.graphdb.Node vertex = null;
		
		Transaction tx = this.db.beginTx();
		try {
			vertex = this.db.getNodeById(id);
			list = getRelations(vertex, Direction.BOTH);
			tx.success();
		} catch (NotFoundException e) {
			tx.failure();
		} catch (Exception e) {
			log.error("could not read relations for node " + id.toString() + ": " + e.toString());
			tx.failure();
		} finally {
			tx.finish();
		}
		
		if (vertex == null) {
			return null;
		}
		
		return list;
	}
	
	
	private ArrayList<TreeMap<Integer,Object>> getRelations(Node vertex, Direction dir) {
		ArrayList<TreeMap<Integer,Object>> list = new ArrayList<TreeMap<Integer,Object>>();
		for (Relationship r : vertex.getRelationships(Direction.BOTH)) {
			list.add(getRelation(r));
		}
		return list;
	}
	
	private TreeMap<Integer,Object> getRelation(Relationship r) {
		TreeMap<Integer,Object> map = new TreeMap<Integer,Object>();
		map.put(1, "edge");
		map.put(2, r.getId());
		map.put(3, r.getStartNode().getId());
		map.put(4, r.getEndNode().getId());
		map.put(5, r.getType().name());
		return map;
	}
	
	
}
