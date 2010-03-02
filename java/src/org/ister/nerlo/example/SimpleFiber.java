package org.ister.nerlo.example;

import java.lang.InterruptedException;

import org.apache.log4j.Logger;
import org.ister.ej.Main;
import org.ister.nerlo.Fiber;
import org.ister.nerlo.AbstractFiber;

/**
 * Really simple fiber.
 * 
 * Thread safe: function object. 
 * 
 * @author ingo
 *
 */
public class SimpleFiber extends AbstractFiber<Long> {
	
	private final Logger log;
	
	public SimpleFiber() {
		super();
		this.log = Main.getLogger();
		this.log.debug("SimpleFiber instantiated");
	}
	
	/**
	 * Sleep ten times for something between 1 and 100 ms,
	 * awake, say what's up and sleep again.
	 * 
	 * Return thread ID.
	 * 
	 * @return
	 */
	public Long call() {
		long id = Thread.currentThread().getId();
		log.debug("call");
		for (int i = 0; i < 10; i++) {
			try {
				long wait = Math.round(Math.random() * 100);
				Thread.sleep(wait);
				System.out.println(id + " awake after " + wait + "ms");
            } catch(InterruptedException e) {
                
            }
        }
		log.debug("call has " + id);
        return new Long(id);
    }
	
	@Override
	public Fiber<Long> getCopy(Fiber<Long> fiber) {
		return new SimpleFiber();
	}


}
