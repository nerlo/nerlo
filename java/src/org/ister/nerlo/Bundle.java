package org.ister.nerlo;

import java.util.ArrayList;
import java.util.concurrent.*;

import org.apache.log4j.Logger;
import org.ister.ej.ConcurrencyUtil;
import org.ister.ej.Main;

/**
 * A bundle subsumes a number of computational threads
 * called Fibers.
 * 
 * Bundles may be subsumed into higher structures such
 * as sheaves or spaces, but these do not have
 * to be represented by Java classes necessarily.
 * 
 * Singleton; because we do not want two thread pools
 * for the same VM (not even for the same machine, but
 * this is left to the admins).
 * 
 * @author ingo
 *
 */
public class Bundle {
	
	private static final Bundle INSTANCE = new Bundle();
	
	private final int n;
	private final Logger log;
	private final ExecutorService exec;
	@SuppressWarnings("unchecked")
	private final CompletionService<Fiber> service;
	
	
	@SuppressWarnings("unchecked")
	private Bundle() {
		this.n = getN();
		this.log = Main.getLogger();
		this.exec = Executors.newFixedThreadPool(n);
		this.service = new ExecutorCompletionService<Fiber>(this.exec);
	}
	
	public static Bundle getInstance() {
		return INSTANCE;
	}
	
	public long getFiberCount() {
		return n;
	}
	
	/**
	 * Fiber will be executed n times concurrently,
	 * where n is the number of fibers in the bundle.
	 * 
	 * @param r
	 */
	@SuppressWarnings("unchecked")
	public ArrayList parallelCopyRun(AbstractFiber fiber) {

		for (int i = 0; i < this.n; i++) {
			this.service.submit(fiber.getCopy(fiber));
		}
		// TODO add result class and parameterize ArrayList that way
		ArrayList r = new ArrayList(this.n);
        for (int i = 0; i < this.n; i++) {
            try {
            	Future<Fiber> fu = this.service.take();
            	r.add(fu.get());
            } catch(ExecutionException e) {
                log.error("Exception: \n" + e.toString());
                ConcurrencyUtil.peelException(e.getCause());
            } catch(InterruptedException e) {
            	log.error("Exception: \n" + e.toString());
            	Thread.currentThread().interrupt();
            }
        }
        
		return r;
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
