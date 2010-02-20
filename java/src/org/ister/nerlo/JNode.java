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
 * so it is fine to have a hidden node. Usually, the peer will even start
 * this node.
 *
 * Once started you may control this node from an Erlang shell node.
 *
 * <pre>
 * $ erl -sname shell -setcookie 123456
 * (shell@host)1> {jnode, 'jnode@host'} ! {self(), {666}}.
 * (shell@host)3> {jnode, 'jnode@host'} ! {self(), {job}}.
 * (shell@host)3> {jnode, 'jnode@host'} ! {self(), {die}}.
 * </pre>
 * 
 * If this has been started canonically from within Erlang with 
 * nerlo_jsrv:start() you may send messages using nerlo_jsrv:send(Msg).
 *
 * @author ingo 
 */
public class JNode {

	private String cookie   = "123456"; // cookie of Erlang cluster
	private String nodename = "jnode";  // name of this node
	private String mboxname = "jnode";  // process registered name (globally?)
	private String peername = "shell";  // name of peer node
	

	private OtpNode node = null;
	private OtpMbox mbox = null;
	
	private Bundle bundle;

	/**
	 *
	 */
	public JNode() throws IOException{
		this.init();
	}

	/**
	 *
	 * @param cookie
	 * @param name
	 * @param peer
	 */
	public JNode(String cookie, String name, String peer) throws IOException {
		this.cookie   = cookie;
		this.nodename = name;
		this.mboxname = name;
		this.peername = peer;
		this.init();
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
                if (o instanceof OtpErlangTuple) {
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
    	// {self(), {die}}    
        if (msg.match(0, new OtpErlangAtom("die"))) {
            shutdown(node);
        // {self(), {job}}
        } else if (msg.match(0, new OtpErlangAtom("job"))) {
            job();
        } else {
            System.out.println("Received from " + msg.getFrom().toString() 
            		+ " message: " + msg.getMsg().toString());
//            this.mbox.send(msg.getFrom(), msg.getMsg());
        }
    }
    
    
    @SuppressWarnings("unchecked")
    public void job() {
        ArrayList<Long> l = bundle.parallelCopyRun(new SimpleFiber());
        for (Long res : l) {
            System.out.println("Future returned: " + res);
        }
    }
    
    
    private void shutdown(OtpNode node) {
        System.out.print("Shutting down...");
        this.bundle.shutdown();
        OtpEpmd.unPublishPort(node);
        System.out.println("bye");
        System.exit(0);
    }
    
    private void init() throws IOException {
		this.node = this.getNode();
		this.bundle = Bundle.getInstance();
		System.out.println("Bundle has " + this.bundle.getFiberCount() + " fibers");
    }

    private OtpNode getNode() throws IOException {
        try {
            OtpNode node = new OtpNode(this.nodename, this.cookie);
            System.out.println("node running: " + this.nodename + "@" + java.net.InetAddress.getLocalHost().getHostName());
            System.out.println("peer: " + this.peername);
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