/**
 * 
 */
package org.ister.ej;

import java.util.HashMap;
import java.util.Map;


import com.ericsson.otp.erlang.*;

/**
 * A java representation of a specific Erlang message.
 * 
 * This message has a well defined format. Construction
 * fails if the format is broken. 
 * 
 * EJMSG := {FROM, REF, {MSG}}
 * FROM  := Pid
 * REF   := {Pid,Ref}
 * MSG   := {TAG,LIST}
 * TAG   := ok | error | data | call | node
 * LIST  := [PART+]
 * PART  := {KEY,VALUE}
 * KEY   := Atom
 * VALUE := Atom | String | Binary | Int 
 * 
 * @author ingo
 *
 */
public class Msg {

	private final OtpErlangPid from;
	private final MsgRef ref;
	private final OtpErlangTuple msg;
	private final Map<String, Object> map;
	
	/**
	 * Create message to send.
	 * 
	 * @param pid
	 * @param tuple
	 */
	public Msg(OtpErlangPid self, MsgRef ref, OtpErlangTuple tuple) {
		this.from = (OtpErlangPid) self.clone();
		this.ref  = (MsgRef) ref.clone();
		this.msg  = (OtpErlangTuple) tuple.clone();
		this.map  = msgToMap();
	}
	
	/**
	 * Create from received message.
	 * 
	 * @param tuple
	 * @throws IllegalArgumentException
	 */
	public Msg(OtpErlangTuple tuple) throws IllegalArgumentException {
		
		if (tuple.arity() != 3) {
			throw new IllegalArgumentException("message has wrong arity");
		}
		
		OtpErlangTuple t = (OtpErlangTuple) tuple.clone();
		this.from = getFrom(t);
		this.ref  = getRef(t);
		this.msg  = getMsg(t);
		this.map  = msgToMap();
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
	
	
	public boolean match(String key, Object value) {
		if (!map.containsKey(key)) {
			return false;
		}
		return value.equals(map.get(key));
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
	 * Get sender Pid of this message.
	 * 
	 * @return
	 */
	public MsgRef getRef() {
		return this.ref;
	}
	
	/**
	 * Get sender Pid of this message.
	 * 
	 * @return
	 */
	public OtpErlangTuple getRefTuple() {
		return this.ref.toTuple();
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
	 * 
	 * @return
	 * @throws IllegalArgumentException
	 */
	public MsgTag getTag() throws IllegalArgumentException {
		OtpErlangObject o = this.msg.elementAt(0);
		if (o instanceof OtpErlangAtom) {
			return new MsgTag(((OtpErlangAtom) o).atomValue());
		}
		throw new IllegalArgumentException("message not properly tagged: " + this.msg.toString());
	}
	
	/**
	 * 
	 * @return
	 */
	public Map<String, Object> msgToMap() throws IllegalArgumentException {
		ErlangTransformer trans = new ErlangTransformer();
		if (this.msg.arity() != 2) {
			throw new IllegalArgumentException("malformed message: wrong arity");
		}
		OtpErlangObject list = this.msg.elementAt(1);
		if (! (list instanceof OtpErlangList)) {
			throw new IllegalArgumentException("malformed message, not a list at position 1");
		}
		int arity = ((OtpErlangList) list).arity();

		HashMap<String, Object> map = new HashMap<String, Object>(arity);
		for (OtpErlangObject t : ((OtpErlangList) list).elements()) {
			if (! (t instanceof OtpErlangTuple)) {
				throw new IllegalArgumentException("malformed message part: not a tuple");
			}
			if (((OtpErlangTuple) t).arity() != 2) {
				throw new IllegalArgumentException("malformed message part: wrong tuple arity");
			}
			String key = ((OtpErlangAtom) ((OtpErlangTuple) t).elementAt(0)).atomValue();
			Object val = trans.toJava(((OtpErlangTuple) t).elementAt(1));
			map.put(key, val);
		}
		return map;
	}
	
	/**
	 * Get message as one single tuple.
	 * 
	 * @return
	 */
	public OtpErlangTuple toTuple() {
		OtpErlangObject[] l = {this.from, this.ref.toTuple(), this.msg}; // new OtpErlangObject[2];
//		l[0] = this.from;
//		l[1] = this.msg;
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
	
	/**
	 * Factory method to create an immutable JMsg from a HashMap.
	 * 
	 * @param map
	 * @param tagstr
	 * @return
	 */
	public static Msg factory(OtpErlangPid self, MsgRef ref, MsgTag msgtag, Map<String, Object> map) {
		ErlangTransformer trans = new ErlangTransformer();
		
		OtpErlangObject[] ts = new OtpErlangObject[map.size()];
		int i = 0;
		for (String key : map.keySet()) {
			OtpErlangObject[] l = new OtpErlangObject[2];
			l[0] = new OtpErlangAtom(key);
			l[1] = trans.fromJava(map.get(key));
			OtpErlangTuple t = new OtpErlangTuple(l);
			ts[i++] = t;
		}
		OtpErlangList list = new OtpErlangList(ts);
		
		OtpErlangObject[] tl = new OtpErlangObject[2];
		tl[0] = msgtag.toAtom();
		tl[1] = list;
		OtpErlangTuple msg = new OtpErlangTuple(tl);
//		MsgRef mref = new MsgRef(self, ref);
		return new Msg(self, ref, msg);
	}
	
	public static Msg answer(OtpErlangPid self, String tag, Map<String, Object> map, Msg request) {
		return factory(self, request.getRef(), new MsgTag(tag), map);
	}
    
	/* PRIVATE */
	
    private OtpErlangPid getFrom(OtpErlangTuple t) throws IllegalArgumentException {
        if (! (t.elementAt(0) instanceof OtpErlangPid)) {
            throw new IllegalArgumentException("cannot determine From");
        }
        return (OtpErlangPid) (t.elementAt(0));
    }
    
    private MsgRef getRef(OtpErlangTuple t) throws IllegalArgumentException {
        if (! (t.elementAt(1) instanceof OtpErlangTuple)) {
            throw new IllegalArgumentException("cannot determine Ref");
        }
        return new MsgRef((OtpErlangTuple) (t.elementAt(1)));
    }
    
    private OtpErlangTuple getMsg(OtpErlangTuple t) throws IllegalArgumentException {
        if (! (t.elementAt(2) instanceof OtpErlangTuple)) {
            throw new IllegalArgumentException("cannot determine Msg");
        }
        return (OtpErlangTuple) (t.elementAt(2));
    }


    
}

