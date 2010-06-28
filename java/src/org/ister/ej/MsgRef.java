package org.ister.ej;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * An ej message reference.
 * 
 * @author ingo
 *
 */
public class MsgRef {

	private final OtpErlangPid pid;
	private final OtpErlangRef ref;
	private final OtpErlangTuple t;
	
	private MsgRef(MsgRef other) {
		this.pid = (OtpErlangPid) other.pid.clone();
		this.ref = (OtpErlangRef) other.ref.clone();
		this.t   = (OtpErlangTuple) other.t.clone();
	}
	
	public MsgRef(OtpErlangPid pid, OtpErlangRef ref) {
		this.pid = (OtpErlangPid) pid.clone();
		this.ref = (OtpErlangRef) ref.clone();
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
		this.pid = (OtpErlangPid) t.elementAt(0).clone();
		this.ref = (OtpErlangRef) t.elementAt(1).clone();
		this.t   = (OtpErlangTuple) t.clone();
	}
	
	public OtpErlangTuple toTuple() {
		return (OtpErlangTuple) this.t.clone();
	}
	
	@Override
	public Object clone() {
		return new MsgRef(this);
	}
	
	@Override
	public boolean equals(Object other) {
		return pid.equals(((MsgRef)other).pid) 
		    && ref.equals(((MsgRef)other).ref) 
		    &&   t.equals(((MsgRef)other).t);
	}
	
}
