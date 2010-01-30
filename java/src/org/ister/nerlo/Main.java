package org.ister.nerlo;

/**
 * Main class.
 * 
 * We should read node spec from command line and/or conf file.
 *
 * @author ingo
 */
public class Main {

    public static void main(String[] args) throws Exception {
        JNode node = new JNode();
        node.run();
    }

}
