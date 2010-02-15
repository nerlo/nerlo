package org.ister.nerlo;

import java.io.IOException;

import java.util.ArrayList;
import java.util.concurrent.Future;
import java.util.concurrent.ExecutionException;

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
 * (shell@host)1> {jnode, 'jnode@host'} ! {self(), 666}.
 * (shell@host)2> receive Any -> Any end.
 * (shell@host)3> {jnode, 'jnode@host'} ! {self(), {job}}.
 * (shell@host)3> {jnode, 'jnode@host'} ! {self(), {die}}.
 * </pre>
 */
public class JNode {

	private String cookie   = "123456"; // cookie of Erlang cluster
	private String nodename = "jnode";  // name of this node
	private String mboxname = "jnode";  // process registered name (globally?)
	private String peername = "shell";  // name of peer node
	

	private OtpNode node = null;
	private OtpMbox mbox = null;

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
                	JMsg msg = new JMsg((OtpErlangTuple) o);
                	processMsg(msg);
                } else {
                	throw new IllegalArgumentException("Tuple expected");
                }
	        } catch (OtpErlangDecodeException e) {
	            System.out.println("Error: received message could not be decoded\n" + e.toString());
	            continue;
	        } catch (OtpErlangExit e) {
	            System.out.println("Error: remote pid " + e.pid() + " has terminated.");
	            continue;
	        } catch (IllegalArgumentException e) {
	        	System.out.println("Error: parsing message\n" + e.toString());
//	        } catch (Exception e) {
//                System.out.println("Error: unexpected exception in while\n" + e.toString());
//                System.exit(1);
            }
        }
    }
	
	public void processMsg(JMsg msg) throws Exception {
       if (msg.match(0, new OtpErlangAtom("die"))) {
    	   shutdown(node);
       } else if (msg.match(0, new OtpErlangAtom("job"))) {
    	   job();
       } else {
    	   System.out.println("Echoing back to: " + msg.getFrom().toString());
    	   this.mbox.send(msg.getFrom(), msg.getMsg());
       }
	}
	
	
	public void job() {
		Bundle b = Bundle.getInstance();
		ArrayList<Future<Long>> l = b.parallelCopyRun(new SimpleFiber());
		for (Future<Long> fu : l) {
			try {
				System.out.println("Future returned: " + fu.get().toString());
			} catch(ExecutionException e) {
				System.out.println("Exception: \n" + e.toString());
			} catch(InterruptedException e) {
				System.out.println("Exception: \n" + e.toString());
			}
		}
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