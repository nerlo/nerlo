package org.ister.graphdb;

import java.util.HashMap;

import org.apache.log4j.Logger;
import org.ister.ej.Main;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.kernel.EmbeddedGraphDatabase;
import org.neo4j.index.IndexService;
import org.neo4j.index.lucene.LuceneIndexService;

public class Db {

	private final Logger log = Main.getLogger();
	private final String path;
	private final String pwd = System.getProperty("user.dir");
	
	private GraphDatabaseService db = null;
	private IndexService index = null;
	
	public Db(String dbpath) {
		this.path = pwd + "/" + dbpath;
	}
	
	public boolean init() {
		try {
			this.db = new EmbeddedGraphDatabase(path);
			this.index = new LuceneIndexService(this.db);
			log.info("graph database initialized: " + path);
		} catch (Exception e) {
			log.error("initialization of database failed: " + e.toString());
			return false;
		}
		return true;
	}
	
	public boolean hasDb() {
		return (this.db != null);
	}
	
	public Long createNode() {
		Long id = null;
		Transaction tx = this.db.beginTx();
		try {
			Node node = this.db.createNode();
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
	
	public boolean deleteNode(Long id) {
		boolean success = false;
		Transaction tx = this.db.beginTx();
		try {
			Node node = this.db.getNodeById(id);
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
	
	public Long addEdge(Long a, Long b, String type, Boolean direction) {
		Long id = null;
		Transaction tx = this.db.beginTx();
		try {
			Node na = this.db.getNodeById(a);
			Node nb = this.db.getNodeById(b);
			RelationshipType rt = null;
			rt = Relations.valueOf(type.toUpperCase());
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
	
	public boolean deleteEdge(Long id) {
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
	
	public void shutdown() {
		if (this.index instanceof IndexService) {
			this.index.shutdown();
		}
		if (this.db instanceof GraphDatabaseService) {
			this.db.shutdown();
		}
	}
}
