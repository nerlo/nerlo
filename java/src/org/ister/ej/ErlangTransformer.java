package org.ister.ej;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import com.ericsson.otp.erlang.*;


/**
 * Transform from OtpErlangObject to Java object and vice versa.
 * 
 * @author ingo
 *
 */
public class ErlangTransformer {

	/**
	 * String becomes atom.
	 * byte[] becomes binary.
	 * Integer becomes int.
	 * Long becomes int.
	 * Double becomes float.
	 * List becomes list.
	 * 
	 * @param o
	 * @return
	 */
	public OtpErlangObject fromJava(Object o) throws IllegalArgumentException {
		
		if (o instanceof String) {
			return new OtpErlangAtom((String) o);
		} else if (o instanceof byte[]) {
			return new OtpErlangBinary((byte[]) o);
		} else if (o instanceof Integer) {
			return new OtpErlangInt(((Integer) o).intValue());
		} else if (o instanceof Long) {
			return new OtpErlangInt(((Long) o).intValue());
		} else if (o instanceof Double) {
			return new OtpErlangDouble(((Double) o).doubleValue());
		} else if (o instanceof List) {
			@SuppressWarnings("unchecked")
			List<Object> list = (List<Object>) o;
			int size = list.size();
			OtpErlangObject[] xs = new OtpErlangObject[size];
			for (int i=0; i<size; i++) {
				// recursion
				xs[i] = fromJava(list.get(i));
			}
			return new OtpErlangList(xs);
		}
		
		throw new IllegalArgumentException("cannot transform input class: " + o.getClass().getName());
	}
	
	/**
	 * Atom becomes String.
	 * Binary becomes byte[].
	 * Int becomes Long.
	 * Float becomes Double.
	 * List becomes ArrayList
	 * 
	 * Note: Since it seems not to be safe to distinguish
	 * Strings from Lists we do not allow Strings. A String
	 * will be handled as a List. Send Strings as Atoms or
	 * Binaries.
	 * 
	 * @param o
	 * @return
	 * @throws IllegalArgumentException
	 */
	public Object toJava(OtpErlangObject o) throws IllegalArgumentException {
		// Logger log = Main.getLogger();
		// log.debug("transform: " + o.getClass().getName());
		if (o instanceof OtpErlangAtom) {
			return ((OtpErlangAtom) o).atomValue();
		} else if (o instanceof OtpErlangBinary) {
			return ((OtpErlangBinary) o).binaryValue();
		}  else if (o instanceof OtpErlangInt) {
			return ((OtpErlangInt) o).longValue();
		} else if (o instanceof OtpErlangLong) {
			return ((OtpErlangLong) o).longValue();	
		} else if (o instanceof OtpErlangDouble) {
			return ((OtpErlangDouble) o).doubleValue();
		} else if (o instanceof OtpErlangString) {
			return toJava(new OtpErlangList(((OtpErlangString) o).stringValue()));
		} else if (o instanceof OtpErlangList) {
			ArrayList<Object> list = new ArrayList<Object>(((OtpErlangList) o).arity());
			for (OtpErlangObject x : ((OtpErlangList) o).elements()) {
				// recursion!
				list.add(toJava(x));
			}
			return list;
		}
		
		throw new IllegalArgumentException("cannot transform input class: " + o.getClass().getName());
	}
	
}
