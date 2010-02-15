package org.ister.nerlo;

import java.util.ArrayList;
import java.util.concurrent.*;

/**
 * A bundle subsumes a number of computational threads
 * called Fibers.
 * 
 * Bundles may be subsumed into higher structures such
 * as superbundles or spaces, but these do not have
 * to be represented by Java classes necessarily.
 * 
 * Singleton; because we do not want two thread pools
 * for the same VM (not even for the same mashine, but
 * this is left to the admins).
 * 
 * @author ingo
 *
 */
public class Bundle {
	
	private static final Bundle INSTANCE = new Bundle();
	
	private final int n;
	private final ExecutorService exec;
	
	
	private Bundle() {
		this.n = getN();
		this.exec = Executors.newFixedThreadPool(n);
	}
	
	public static Bundle getInstance() {
		return INSTANCE;
	}
	
	/**
	 * Fiber will be executed n times concurrently,
	 * where n is the number of fibers in the bundle.
	 * 
	 * @param r
	 */
	public ArrayList parallelCopyRun(Fiber fib) {
		ArrayList<Future> l = new ArrayList<Future>(this.n);
		for (int i = 0; i < this.n; i++) {
			Future<Future> future = this.exec.submit((Fiber)fib.clone());
			l.add(future);
		}
		return l;
	}
	
	
	public void shutdown() {
		this.exec.shutdown();
	}
	
	
	public void kill() {
		this.exec.shutdownNow();
	}
	
	
	private int getN() {
		// later we may add a more sophisticated
		// computation here
		return Runtime.getRuntime().availableProcessors();
	}

}
