package org.ister.ej;

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
	 * StringBuffer becomes list.
	 * byte[] becomes binary.
	 * Integer becomes int.
	 * 
	 * @param o
	 * @return
	 */
	public OtpErlangObject fromJava(Object o) throws IllegalArgumentException {
		if (o instanceof String) {
			return new OtpErlangAtom((String) o);
		} else if (o instanceof byte[]) {
			return new OtpErlangBinary((byte[]) o);
		} else if (o instanceof String) {
			return new OtpErlangString((String) o);
		} else if (o instanceof Integer) {
			return new OtpErlangInt(((Integer) o).intValue());
		} else if (o instanceof Long) {
			return new OtpErlangInt(((Long) o).intValue());
		}
		
		throw new IllegalArgumentException("cannot transform input class: " + o.getClass().getName());
	}
	
	/**
	 * Atom becomes String.
	 * Binary becomes byte[].
	 * String becomes String.
	 * Int becomes long.
	 * 
	 * @param o
	 * @return
	 * @throws IllegalArgumentException
	 */
	public Object toJava(OtpErlangObject o) throws IllegalArgumentException {
		if (o instanceof OtpErlangAtom) {
			return ((OtpErlangAtom) o).atomValue();
		} else if (o instanceof OtpErlangBinary) {
			return ((OtpErlangBinary) o).binaryValue();
		} else if (o instanceof OtpErlangString) {
			return ((OtpErlangString) o).stringValue();
		} else if (o instanceof OtpErlangInt) {
			return ((OtpErlangInt) o).longValue();
		}
		
		throw new IllegalArgumentException("cannot transform input class: " + o.getClass().getName());
	}
	
}
