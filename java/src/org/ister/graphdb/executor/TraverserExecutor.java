package org.ister.graphdb.executor;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.graphdb.Relations;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.graphdb.ReturnableEvaluator;
import org.neo4j.graphdb.StopEvaluator;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.Traverser;
import org.neo4j.index.IndexHits;

public class TraverserExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return (msg.has("id"));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		traverse(msg);
		return Msg.lastFragment(node.getSelf(), msg);
	}

	@Override
	protected String getId() {
		return "traverse";
	}
	
	private void send(long id, Msg msg) {
		Map<String, Object> map = Collections.singletonMap("result",(Object)new Long(id));
		Msg next = Msg.fragment(node.getSelf(), map, msg);
		this.node.sendPeer(next);
	}
	
	private void traverse(Msg msg) throws ExecutorException {
		long start = ((Long)msg.get("id")).longValue();
		Transaction tx = this.db.beginTx();
		try {
			org.neo4j.graphdb.Node startNode = this.db.getNodeById(start);
			Traverser t = startNode.traverse(Traverser.Order.BREADTH_FIRST,
											 StopEvaluator.END_OF_GRAPH,
											 ReturnableEvaluator.ALL_BUT_START_NODE,
											 Relations.EDGE,
											 Direction.BOTH);
			Iterator<org.neo4j.graphdb.Node> i = t.iterator();
			while (i.hasNext()) {
				org.neo4j.graphdb.Node next = i.next();
				send(next.getId(), msg);
			}
			tx.success();
//		} catch (ExecutorException e) {
//			throw e;
		} catch (NotFoundException e) {
			throw new ExecutorException("start_vertex_not_found");
		} catch (Exception e) {
			log.error("exception in traversal: " + e.toString());
			tx.failure();
			throw new ExecutorException("exception_in_traversal");
		} finally {
			tx.finish();
		}
	}

}
