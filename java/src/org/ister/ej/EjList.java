package org.ister.ej;

import java.util.List;

/**
 * This interface represents a functional list type.
 * 
 * @author ingo
 *
 * @param <E>
 */
public interface EjList extends List<Object> {

	public List<Object> toList();
	
}
