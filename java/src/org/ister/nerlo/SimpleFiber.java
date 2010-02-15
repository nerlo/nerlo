package org.ister.nerlo;

/**
 * 
 * @author ingo
 *
 */
public class SimpleFiber implements Fiber {
	
//	private int count = 0;
	
//	public SimpleFiber() {
//		
//	}
	
	public Long call() {
		return new Long(Thread.currentThread().getId());
	}
	
	
	@Override public SimpleFiber clone() {
		try {
			return (SimpleFiber) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new AssertionError();
		}
	}

}
