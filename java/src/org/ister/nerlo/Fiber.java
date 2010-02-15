package org.ister.nerlo;

import java.util.concurrent.*;

/**
 * 
 * @author ingo
 *
 * @param <V>
 */
public interface Fiber<V> extends Callable, Cloneable {
    
    public Object clone();
}
