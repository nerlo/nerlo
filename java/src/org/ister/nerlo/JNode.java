package org.ister.nerlo;

import java.io.IOException;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.ister.nerlo.example.SimpleFiber;

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

	private static JNode INSTANCE = null;
	
	private final String cookie;
	private final String nodename;
	private final String mboxname;
	private final String peernode;
	private final Logger log;
	
	private OtpNode node = null;
	private OtpMbox mbox = null;
	
	private OtpErlangPid peerpid  = null;
	private Bundle bundle;

	/**
	 * Create with custom setup.
	 *
	 * @param cookie
	 * @param name
	 * @param peer
	 */
	private JNode(String name, String peer, Properties props) throws IOException {
		this.cookie   = Main.getProperty("jnode.cookie", null);
		this.log      = Main.getLogger();
		this.nodename = name;
		this.mboxname = name;
		this.peernode = peer;
		this.node = this.getNode();
		this.bundle = Bundle.getInstance();
	}
	
	/**
	 * Get instance.
	 * 
	 * @param cookie
	 * @param name
	 * @param peer
	 * @return
	 * @throws IOException
	 */
	public static JNode getInstance(String name, String peer, Properties props)  throws IOException {
		if (INSTANCE == null) {
			INSTANCE = new JNode(name, peer, props);
		}
		return INSTANCE;
	}

	/**
	 * Run server loop.
	 *
	 * @throws Exception
	 */
	public void run() throws Exception {
		
        if (node.ping(peernode, 2000)) {
            log.info(peernode + ": pong.");
        } else {
        	// to die or not to die ...
            log.warn(peernode + ": pang!");
        }

        while (true) {
            try {
                OtpErlangObject o = this.mbox.receive();
                if (o instanceof OtpErlangTuple) {
                	JMsg msg = new JMsg((OtpErlangTuple) o);
                	// TODO only allow messages from peer
                	processMsg(msg);
                } else {
                	throw new IllegalArgumentException("Tuple expected");
                }
	        } catch (OtpErlangDecodeException e) {
	            log.error("received message could not be decoded\n" + e.toString());
	            continue;
	        } catch (OtpErlangExit e) {
	            log.error("remote pid " + e.pid() + " has terminated.");
	            continue;
	        } catch (IllegalArgumentException e) {
	        	log.error("parsing message\n" + e.toString());
//	        } catch (Exception e) {
//                System.out.println("Error: unexpected exception in while\n" + e.toString());
//                System.exit(1);
            }
        }
    }
    
    
    /* PRIVATE */

	
    private void processMsg(JMsg msg) throws Exception {
    	// {self(), {handshake}}    
        if        (msg.match(0, new OtpErlangAtom("handshake"))) {
            handshake(msg);
    	// {self(), {die}}    
        } else if (msg.match(0, new OtpErlangAtom("die"))) {
            shutdown(node);
        // {self(), {job}}
        } else if (msg.match(0, new OtpErlangAtom("job"))) {
            job();
        } else {
            log.debug("Received from " + msg.getFrom().toString() 
            		+ " message: " + msg.getMsg().toString());
        }
    }
    
    private void handshake(JMsg msg) {
    	if (this.peerpid != null) return;
    	// dangerous; more sender checks needed
    	this.peerpid = msg.getFrom();
    	log.info("handshake from: " + this.peerpid.toString());
    	sendPeer(new OtpErlangTuple(new OtpErlangAtom("handshake")));
    }
    
    private void shutdown(OtpNode node) {
        log.info("Shutting down...");
        this.bundle.shutdown();
        sendPeer(new OtpErlangTuple(new OtpErlangAtom("bye")));
        OtpEpmd.unPublishPort(node);
        log.info("...bye");
        System.exit(0);
    }

    
    @SuppressWarnings("unchecked")
    private void job() {
        List<Long> l = bundle.parallelCopyRun(new SimpleFiber());
        for (Long res : l) {
            log.info("Future returned: " + res);
        }
    }
    
    private void sendPeer(OtpErlangTuple t) {
    	JMsg msg = new JMsg(this.mbox.self(), t);
    	if (this.peerpid == null) {
    		log.error("cannot send, have no pid of peer");
    		return;
    	}
    	this.mbox.send(this.peerpid, msg.toTuple());
    }

    private OtpNode getNode() throws IOException {
        try {
            OtpNode node = new OtpNode(this.nodename, this.cookie);
            log.info("node running: " + this.nodename + "@" + java.net.InetAddress.getLocalHost().getHostName());
            log.info("peer: " + this.peernode);
            if (OtpEpmd.publishPort(node)) {
                log.info("Node registered");
            } else {
                log.warn("Warning: Node registration failed");
            }
            String[] names = OtpEpmd.lookupNames();
            for (String name: names) {
                log.debug(name);
            }
            this.mbox = node.createMbox(this.mboxname);
            log.debug("self: " + this.mbox.self());
            return node;
        } catch (IOException e) {
            log.fatal("no node\n" + e.toString());
            throw e;
        }
    }

}