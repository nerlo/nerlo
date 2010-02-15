package org.ister.nerlo;

import java.lang.InterruptedException;

/**
 * Really simple fiber.
 * 
 * @author ingo
 *
 */
public class SimpleFiber implements Fiber {
	
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
		for (int i = 0; i < 10; i++) {
			try {
				long wait = Math.round(Math.random() * 100);
				Thread.sleep(wait);
				System.out.println(id + " awake after " + wait + "ms");
            } catch(InterruptedException e) {
                
            }
        }
        return new Long(id);
    }
    
    
    @Override public SimpleFiber clone() {
        try {
            return (SimpleFiber) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }

}
