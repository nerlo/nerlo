package org.ister.ej;

import com.ericsson.otp.erlang.OtpErlangAtom;

/**
 * An Erlang message tag.
 * 
 * @author ingo
 *
 */
public class MsgTag {
	
	private final String tag;
	
	public static final String OK    = "ok";
	public static final String ERROR = "error";
	public static final String DATA  = "data";
	public static final String CALL  = "call";
	public static final String NODE  = "node";
	
	/**
	 * 
	 * @param tag
	 * @throws IllegalArgumentException
	 */
	public MsgTag(String tag) throws IllegalArgumentException {
		if (!(tag.equals(OK) 
			|| tag.equals(ERROR) 
			|| tag.equals(DATA) 
			|| tag.equals(CALL)
			|| tag.equals(NODE))) {
			throw new IllegalArgumentException("tag not allowed: " + tag);
		}
		this.tag = tag;
	}
	
	/**
	 * 
	 * @return
	 */
	public OtpErlangAtom toAtom() {
		return new OtpErlangAtom(this.tag);
	}
	
	/**
	 * Return string representation of the tag.
	 */
	public String toString() {
		return this.tag;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof MsgTag) {
			return (this.tag == ((MsgTag) other).tag);
		} else if (other instanceof String) {
			return (this.tag.equals(other));
		} else {
			return false;
		}
	}
}
