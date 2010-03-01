package org.ister.nerlo;

import com.ericsson.otp.erlang.OtpErlangAtom;

/**
 * An Erlang message tag.
 * 
 * @author ingo
 *
 */
public class ErlangMsgTag {
	
	private final String tag;
	
	public static final String OK = "ok";
	public static final String ERROR = "error";
	public static final String DATA = "data";
	
	/**
	 * 
	 * @param tag
	 * @throws IllegalArgumentException
	 */
	public ErlangMsgTag(String tag) throws IllegalArgumentException {
		if (!(tag.equals(OK) || tag.equals(ERROR) || tag.equals(DATA))) {
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
		if (other instanceof ErlangMsgTag) {
			return (this.tag == ((ErlangMsgTag) other).tag);
		} else if (other instanceof String) {
			return (this.tag.equals(other));
		} else {
			return false;
		}
	}
}
