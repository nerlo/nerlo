package org.ister.ej;

import static org.junit.Assert.*;

import java.sql.Ref;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.*;

import org.ister.ej.MsgTag;

public class MsgTest {

	private final OtpErlangString ok    = new OtpErlangString(MsgTag.OK);
	private final OtpErlangString call  = new OtpErlangString(MsgTag.CALL);
	private final OtpErlangList payload = new OtpErlangList();
	private final OtpErlangObject[] os  = new OtpErlangObject[2];
	
	private final OtpErlangPid pid      = new OtpErlangPid("foo",0,0,0);
	
	private OtpErlangTuple tuple = null;
	
//	private final OtpErlangString s2 = new OtpErlangString("2");
//	private final OtpErlangString s3 = new OtpErlangString("3");
//	private final OtpErlangString s4 = new OtpErlangString("4");
	private final MsgRef ref1 = new MsgRef(new OtpErlangPid("foo",0,0,0), new OtpErlangRef("dummy", 0, 0));
	private final MsgRef ref2 = new MsgRef(new OtpErlangPid("foo",1,0,0), new OtpErlangRef("dummy", 1, 0));
	
	@Before
	public void setUp() throws Exception {
		os[0] = ok;
		os[1] = payload;
		tuple = new OtpErlangTuple(os);
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testHashCode() {
		Msg m1 = new Msg(pid, ref1, tuple);
		Msg m2 = new Msg(pid, ref2, tuple);
	    assertTrue(m1.hashCode() != m2.hashCode());
	}

	@Test
	public void testMsgOtpErlangPidMsgRefOtpErlangTuple() {
		Msg m1 = new Msg(pid, ref1, tuple);
		assertTrue(m1 instanceof Msg);
	}

	@Test
	public void testMsgOtpErlangTuple() {
		OtpErlangObject[] ref  = new OtpErlangObject[2];
		ref[0] = pid;
		ref[1] = new OtpErlangRef("dummy", 0, 0);
		OtpErlangObject[] m  = new OtpErlangObject[3];
		m[0] = pid;
		m[1] = new OtpErlangTuple(ref);
		m[2] = tuple;
		OtpErlangTuple t = new OtpErlangTuple(m);
		Msg m1 = new Msg(t);
		assertTrue(m1 instanceof Msg);
	}

	@Test
	public void testMatchIntOtpErlangObject() {
		Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", new Integer(1));
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertTrue(m.match(0, new OtpErlangAtom(MsgTag.OK)));
	}

	@Test
	public void testMatchStringObject() {
		Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "foo");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertTrue(m.match("call", "foo"));
        assertFalse(m.match("call", 2));
	}

	@Test
	public void testHas() {
		Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertTrue(m.has("call"));
        assertFalse(m.has("dummy"));
	}

	@Test
	public void testGet() {
    	Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertEquals(m.get("call"), "me");
	}

	@Test
	public void testGetFrom() {
		Msg m1 = new Msg(pid, ref1, tuple);
		assertTrue(m1.getFrom().node() == "foo");
	}

	@Test
	public void testGetRefGetRefTuple() {
		Msg m1 = new Msg(pid, ref1, tuple);
		assertTrue(m1.getRef().toTuple().equals(m1.getRefTuple()));
	}

	@Test
	public void testGetMsg() {
		Msg m1 = new Msg(pid, ref1, tuple);
		assertTrue(m1.getMsg().equals(tuple));
	}

	@Test
	public void testGetMap() {
		Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertTrue(m.getMap().equals(map));
	}

	@Test
	public void testGetTag() {
		Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertTrue(m.getTag().equals(MsgTag.OK));		
	}

	@Test
	public void testToTuple() {
		Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertTrue(m.toTuple() instanceof OtpErlangTuple);		
	}

	@Test
	public void testToString() {
		Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertTrue(m.toString() instanceof java.lang.String);		
	}

	@Test
	public void testFactory() {
    	Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        assertTrue(m instanceof Msg);
	}

	@Test
	public void testAnswer() {
    	Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        Msg a = Msg.answer(m.getFrom(), MsgTag.OK, map, m);
        assertTrue(m.getFrom().equals(a.getFrom()));
        assertTrue(m.getRef().equals(a.getRef()));
	}

	@Test
	public void testFragment() {
    	Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        Msg a = Msg.fragment(m.getFrom(), map, m);
        assertTrue(a.getTag().equals(MsgTag.FRAGMENT));
	}

	@Test
	public void testLastFragment() {
    	Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("call", "me");
        Msg m = Msg.factory(pid, ref1, new MsgTag(MsgTag.OK), map);
        Msg a = Msg.lastFragment(m.getFrom(), m);
        assertTrue(a.getTag().equals(MsgTag.OK));
        assertTrue(a.match("result", "EJCALLBACKSTOP"));
	}

}
