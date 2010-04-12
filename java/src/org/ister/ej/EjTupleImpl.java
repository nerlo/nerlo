package org.ister.ej;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * 
 * @author ingo
 *
 */
public class EjTupleImpl extends TreeMap<Integer,Object> implements EjTuple {

	private static final long serialVersionUID = 771508555642273979L;

	/**
	 * 
	 */
	@Override
	public List<Object> toList() {
		List<Object> list = new ArrayList<Object>(this.size());
		for (Entry<Integer,Object> e : this.entrySet()) {
			list.add(((Integer)e.getKey()).intValue(), e.getValue());
		}
		return list;
	}

	/**
	 * 
	 */
	@Override
	public Map<Integer, Object> toMap() {
		TreeMap<Integer,Object> map = new TreeMap<Integer,Object>();
		map.putAll(this);
		return map;
	}
	
}
