package org.ister.graphdb.executor;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;

public class TraverserExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return true; //TODO
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		for (int i=0; i<3; i++) {
			Map<String, Object> map = Collections.singletonMap("result",(Object)new Integer(i));
			Msg next = Msg.fragment(node.getSelf(), map, msg);
			this.node.sendPeer(next);
		}
		return Msg.lastFragment(node.getSelf(), msg);
	}

	@Override
	protected String getId() {
		return "traverse";
	}

}
