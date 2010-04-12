/**
 * 
 */
package org.ister.ej;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * @author ingo
 *
 */
public class ErlangTransformerTest {

	private final ErlangTransformer tf = new ErlangTransformer();
	
	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link org.ister.ej.ErlangTransformer#fromJava(java.lang.Object)}.
	 */
	@Test
	public void testFromJavaEjList() {
		EjList in = new EjListImpl();
		in.add(1);
		in.add("foo");
		OtpErlangList out = (OtpErlangList) tf.fromJava(in);
		assertTrue(out instanceof OtpErlangList);
		assertEquals(out.arity(), 2);
	}
	
	/**
	 * Test method for {@link org.ister.ej.ErlangTransformer#fromJava(java.lang.Object)}.
	 */
	@Test
	public void testFromJavaEjMap() {
		EjTuple in = new EjTupleImpl();
		in.put(1, "foo");
		in.put(2, 3);
		OtpErlangTuple out = (OtpErlangTuple) tf.fromJava(in);
		assertTrue(out instanceof OtpErlangTuple);
		assertEquals(out.arity(), 2);
	}
	
	/**
	 * Test method for {@link org.ister.ej.ErlangTransformer#fromJava(java.lang.Object)}.
	 */
	@Test
	public void testFromJavaList() {
		ArrayList<Object> in = new ArrayList<Object>();
		in.add(1);
		in.add("foo");
		OtpErlangList out = (OtpErlangList) tf.fromJava(in);
		assertTrue(out instanceof OtpErlangList);
		assertEquals(out.arity(), 2);
	}
	
	/**
	 * Test method for {@link org.ister.ej.ErlangTransformer#fromJava(java.lang.Object)}.
	 */
	@Test
	public void testFromJavaMap() {
		Map<Integer,Object> in = new TreeMap<Integer,Object>();
		in.put(1, "foo");
		in.put(2, 3);
		OtpErlangTuple out = (OtpErlangTuple) tf.fromJava(in);
		assertTrue(out instanceof OtpErlangTuple);
		assertEquals(out.arity(), 2);
	}

	/**
	 * Test method for {@link org.ister.ej.ErlangTransformer#toJava(com.ericsson.otp.erlang.OtpErlangObject)}.
	 */
	@Test
	public void testToJavaOtpErlangList() {
		OtpErlangObject[] os = {new OtpErlangAtom("foo"), new OtpErlangAtom("bar")};
		OtpErlangList in = new OtpErlangList(os);
		EjList out = (EjList) tf.toJava(in);
		assertTrue(out instanceof EjList);
		assertTrue(out instanceof List<?>);
		assertTrue(out.toList() instanceof List<?>);
		assertEquals(out.size(),2);
	}
	
	/**
	 * Test method for {@link org.ister.ej.ErlangTransformer#toJava(com.ericsson.otp.erlang.OtpErlangObject)}.
	 */
	@Test
	public void testToJavaOtpErlangTuple() {
		OtpErlangObject[] os = {new OtpErlangAtom("foo"), new OtpErlangAtom("bar")};
		OtpErlangTuple in = new OtpErlangTuple(os);
		EjTuple out = (EjTuple) tf.toJava(in);
		assertTrue(out instanceof EjTuple);
		assertTrue(out instanceof Map<?,?>);
		assertTrue(out.toList() instanceof List<?>);
		assertTrue(out.toMap() instanceof Map<?,?>);
		assertEquals(out.size(),2);
	}

}
