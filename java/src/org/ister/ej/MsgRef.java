package org.ister.ej;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * An ej message ref.
 * 
 * @author ingo
 *
 */
public class MsgRef {

	private final OtpErlangPid pid;
	private final OtpErlangRef ref;
	private final OtpErlangTuple t;
	
	public MsgRef(OtpErlangPid pid, OtpErlangRef ref) {
		this.pid = pid;
		this.ref = ref;
		OtpErlangObject[] r = {this.pid, this.ref};
		this.t   = new OtpErlangTuple(r);
	}
	
	public MsgRef(OtpErlangTuple t) throws IllegalArgumentException {
		if (t.arity() != 2) {
			throw new IllegalArgumentException("ref tuple has wrong arity");
		}
		if (! (t.elementAt(0) instanceof OtpErlangPid)) {
            throw new IllegalArgumentException("ref has no pid at position 0");
        }
		if (! (t.elementAt(1) instanceof OtpErlangRef)) {
            throw new IllegalArgumentException("ref has no ref at position 1");
        }
		this.pid = (OtpErlangPid) t.elementAt(0);
		this.ref = (OtpErlangRef) t.elementAt(1);
		this.t   = t;
	}
	
	public OtpErlangTuple toTuple() {
		return this.t;
	}
	
}
