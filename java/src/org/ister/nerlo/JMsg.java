/**
 * 
 */
package org.ister.nerlo;

import com.ericsson.otp.erlang.*;

/**
 * A java representation of a specific Erlang message.
 * 
 * This message has a well defined format. Construction
 * fails if the format is broken. A valid form is at
 * least like this: {From, {Msg}}
 * 
 * Should be threadsafe. But, what exactly does "threadsafe" mean...?
 * This one is immutable.
 * 
 * @author ingo
 *
 */
public class JMsg {

	private final OtpErlangPid from;
	private final OtpErlangTuple msg;
	
	/**
	 * Create message to send.
	 * 
	 * @param pid
	 * @param tuple
	 */
	public JMsg(OtpErlangPid self, OtpErlangTuple tuple) {
		this.from = self;
		this.msg  = tuple;
	}
	
	/**
	 * Create from received message.
	 * 
	 * @param tuple
	 * @throws IllegalArgumentException
	 */
	public JMsg(OtpErlangTuple tuple) throws IllegalArgumentException {
		
		if (tuple.arity() != 2) {
			throw new IllegalArgumentException("cannot determine From");
		}
		OtpErlangTuple t = (OtpErlangTuple) tuple.clone();
		this.from = getFrom(t);
		this.msg  = getMsg(t);
		
	}
	
	
	/**
	 * This is for matching a tuple element of the message 
	 * with a match spec.
	 *
	 * @param pos
	 * @param match
	 * @return
	 */
	public boolean match(int i, OtpErlangObject match) {
        if (this.msg.arity() >= i && this.msg.elementAt(i).getClass().getName() == match.getClass().getName()) {
        	if (this.msg.elementAt(i).equals(match)) {
        		return true;
        	}
        }
		return false;
	}
	
	/**
	 * Get sender Pid of this message.
	 * 
	 * @return
	 */
	public OtpErlangPid getFrom() {
		return this.from;
	}
	
	/**
	 * Get message body of this message.
	 * 
	 * @return
	 */
	public OtpErlangTuple getMsg() {
		return this.msg;
	}
	
	/**
	 * Get message as one single tuple.
	 * 
	 * @return
	 */
	public OtpErlangTuple toTuple() {
		OtpErlangObject[] l = new OtpErlangObject[2];
		l[0] = this.from;
		l[1] = this.msg;
		return new OtpErlangTuple(l);
	}
	
	
	/**
	 * Tuple element at position, starting with 0.
	 * 
	 * @param i
	 * @return
	 */
	public OtpErlangObject elementAt(int i) {
		return this.msg.elementAt(i);
	}
    
	/* PRIVATE */
	
    private OtpErlangPid getFrom(OtpErlangTuple t) throws IllegalArgumentException {
        if (! (t.elementAt(0) instanceof OtpErlangPid)) {
            throw new IllegalArgumentException("cannot determine From");
        }
        return (OtpErlangPid) (t.elementAt(0));
    }
    
    private OtpErlangTuple getMsg(OtpErlangTuple t) throws IllegalArgumentException {
        if (! (t.elementAt(1) instanceof OtpErlangTuple)) {
            throw new IllegalArgumentException("cannot determine Msg");
        }
        return (OtpErlangTuple) (t.elementAt(1));
    }    
    
}
