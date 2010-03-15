package org.ister.graphdb;

import org.apache.log4j.Logger;
import org.ister.ej.Main;
import org.neo4j.graphdb.GraphDatabaseService;
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
	
	
	public void shutdown() {
		if (this.index instanceof IndexService) {
			this.index.shutdown();
		}
		if (this.db instanceof GraphDatabaseService) {
			this.db.shutdown();
		}
	}
}
