package org.ister.ej;

/**
 * 
 * @author ingo
 *
 */
public class ConcurrencyUtil {

	public static RuntimeException peelException(Throwable e) {
		if (e instanceof RuntimeException) {
			return (RuntimeException) e;
		} else if (e instanceof Error) {
			throw (Error) e;
		} else {
			throw new IllegalStateException("Peel failed: ", e);
		}
	}
}
