package org.ister.ej;

import java.util.List;
import java.util.Map;

/**
 * This interface represents a functional tuple type.
 * 
 * @author ingo
 *
 * @param <Integer>
 * @param <V>
 */
public interface EjTuple extends Map<Integer, Object> {

	public List<Object> toList();
	
	public Map<Integer,Object> toMap();
	
}
