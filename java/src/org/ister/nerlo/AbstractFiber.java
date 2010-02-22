package org.ister.nerlo;

public abstract class AbstractFiber<V> implements Fiber<V> {

	public abstract Fiber<V> getCopy(Fiber<V> fiber);
	
}
