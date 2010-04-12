package org.ister.ej;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * 
 * @author ingo
 *
 */
public class EjListImpl extends ArrayList<Object> implements EjList {

	private static final long serialVersionUID = 5267716880338962021L;

	/**
	 * 
	 */
	public EjListImpl() {
		super();
	}
	
	/**
	 * 
	 * @param length
	 */
	public EjListImpl(int length) {
		super(length);
	}
	
	/**
	 * 
	 */
	public List<Object> toList() {
		ArrayList<Object> list = new ArrayList<Object>(this.size());
		list.addAll(this);
		return list;
	}
	
}
