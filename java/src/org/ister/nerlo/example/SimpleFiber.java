package org.ister.nerlo.example;

import java.lang.InterruptedException;

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
	
	@Override
	public Fiber<Long> getCopy(Fiber<Long> fiber) {
		return new SimpleFiber();
	}


}
