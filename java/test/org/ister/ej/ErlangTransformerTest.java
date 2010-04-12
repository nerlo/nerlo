/**
 * 
 */
package org.ister.ej;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
		OtpErlangList out = (OtpErlangList) tf.fromJava(in);
		assertTrue(out instanceof OtpErlangList);
	}
	
	/**
	 * Test method for {@link org.ister.ej.ErlangTransformer#fromJava(java.lang.Object)}.
	 */
	@Test
	public void testFromJavaEjMap() {
		EjTuple in = new EjTupleImpl();
		OtpErlangTuple out = (OtpErlangTuple) tf.fromJava(in);
		assertTrue(out instanceof OtpErlangTuple);
	}
	
	/**
	 * Test method for {@link org.ister.ej.ErlangTransformer#fromJava(java.lang.Object)}.
	 */
	@Test
	public void testFromJavaList() {
		ArrayList<Object> in = new ArrayList<Object>();
		OtpErlangList out = (OtpErlangList) tf.fromJava(in);
		assertTrue(out instanceof OtpErlangList);
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
	}

}
