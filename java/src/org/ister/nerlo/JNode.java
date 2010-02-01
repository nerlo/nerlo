package org.ister.nerlo;

import java.io.IOException;
import com.ericsson.otp.erlang.*;

/**
 * This is always a hidden node in the cluster (due to jinterface).
 *
 * We aim only to have this node available to one particular peer node
 * so it is fine to have a hidden node. Later the peer will even start
 * this node.
 *
 * @author ingo
 *
 * <pre>
 * $ erl -sname shell -setcookie 123456
 * (shell@host)1> {echo, 'echo@host'} ! {self(), 666}.
 * (shell@host)2> receive Any -> Any end.
 * (shell@host)3> {echo, 'echo@host'} ! {self(), die}.
 * </pre>
 */
public class JNode {

	String cookie   = "123456"; // cookie of Erlang cluster
	String nodename = "jnode";  // name of this node
	String mboxname = "jnode";  // process registered name (globally?)
	String peername = "shell";  // name of peer node

	OtpNode node = null;
	OtpMbox mbox = null;

	/**
	 *
	 */
	public JNode() throws Exception{
		this.node = this.getNode();
	}

	/**
	 *
	 * @param cookie
	 * @param name
	 * @param peer
	 */
	public JNode(String cookie, String name, String peer) throws Exception {
		this.cookie   = cookie;
		this.nodename = name;
		this.mboxname = name;
		this.peername = peer;
		this.node = this.getNode();
	}

	/**
	 * Run server loop.
	 *
	 * @throws Exception
	 */
	public void run() throws Exception {

        if (node.ping(peername, 2000)) {
            System.out.println(peername + ": pong.");
        } else {
        	// to die or not to die ...
            System.out.println(peername + ": pang!");
        }

        while (true) {
            try {
                OtpErlangObject o = this.mbox.receive();
                System.out.println("Received something from somewehere.");
                if (o instanceof OtpErlangTuple) {
                	System.out.println("got tuple: " + o.toString());
                	OtpErlangTuple t = (OtpErlangTuple) o;
                	OtpErlangPid from = this.getFrom(t);
                	this.processMsg(from, t);
                } else {
                	System.out.println("Error: tuple expected");
                }
	        } catch (OtpErlangDecodeException e) {
	            System.out.println("Error: received message could not be decoded\n" + e.toString());
	            continue;
	        } catch (OtpErlangExit e) {
	            System.out.println("Error: remote pid " + e.pid() + " has terminated.");
	            continue;
	        } catch (Exception e) {
                System.out.println("Error: unexpected exception in while\n" + e.toString());
                System.exit(1);
            }
        }
    }

	/**
	 *
	 * @param t
	 */
	public void processMsg(OtpErlangPid from, OtpErlangTuple t) throws Exception {
        if (this.match(t, 1, new OtpErlangAtom("die"))) {
        	this.shutdown(node);
        }
        System.out.println("Echoing back to: " + from.toString());
        this.mbox.send(from, t.elementAt(1));
	}


	private OtpErlangPid getFrom(OtpErlangTuple t) throws Exception {
		if (t.arity() < 1) {
			throw new Exception("cannot determine From");
		}
		if (! (t.elementAt(0) instanceof OtpErlangPid)) {
			throw new Exception("cannot determine From");
        }
		return (OtpErlangPid) (t.elementAt(0));
	}

	/**
	 * This is for matching a tuple element with a match spec.
	 *
	 * In Erlang BTW simply some "{_,B,_,_} = Tuple" or similar.
	 *
	 * @param t
	 * @param pos
	 * @param match
	 * @return
	 */
	public boolean match(OtpErlangTuple t, int pos, OtpErlangObject match) {
        if (t.arity() == pos+1 && t.elementAt(pos).getClass().getName() == match.getClass().getName()) {
        	if (t.elementAt(pos).equals(match)) {
        		return true;
        	}
        }
		return false;
	}


	private void shutdown(OtpNode node) {
		OtpEpmd.unPublishPort(node);
		System.out.println("Shutting down...");
		System.exit(0);
	}

	private OtpNode getNode() throws Exception {
    	try {
    		OtpNode node = new OtpNode(this.nodename, this.cookie);
    		System.out.println("node running: " + this.nodename);
    		if (OtpEpmd.publishPort(node)) {
    			System.out.println("Node registered");
    		} else {
    			System.out.println("Warning: Node registration failed");
    		}
    		String[] names = OtpEpmd.lookupNames();
    		for (String name: names) {
    			System.out.println(name);
    		}
    		this.mbox = node.createMbox(this.mboxname);
    		return node;
	    } catch (IOException e) {
	    	System.out.println("Fatal: no node\n" + e.toString());
	    	throw e;
        }
	}
}