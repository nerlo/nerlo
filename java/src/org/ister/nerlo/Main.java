package org.ister.nerlo;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

import org.apache.commons.cli.*;

/**
 * Main class.
 * 
 * We should read node spec from command line and/or conf file.
 *
 * @author ingo
 */
public class Main {

	private final String[] args;
	private final String pwd = System.getProperty("user.dir");
	
	private String sname  = "jnode";
	private String cookie = "123456";
	private String peer   = "shell";
	private String propf  = pwd + "/properties";
	
	private static JNode NODE = null;
	private static Properties PROPERTIES = null;
	
	private static final String VERSION = "0.0.1-alpha";
	private static final String NAME = "nerlo";
	
	private Main(String[] args) {
		this.args = args;
	}
	
	/**
	 * Run the program.
	 * 
	 * @throws Exception
	 */
	public void run() throws Exception {
	    parseOptions(this.args);
	    initProps(this.propf);
        NODE = JNode.getInstance(cookie, sname, peer);
        NODE.run();		
	}
	
	private void parseOptions(String[] args) {
	    CommandLineParser parser = new GnuParser();
	    try {
	        Options options = getOptions();
	        CommandLine line = parser.parse( options, args );
	        processCommandLine(line, options);
	    } catch( ParseException e ) {
	        System.out.println( "Parsing command line failed.  Reason: " + e.getMessage() );
	    }
	}
	
	private void processCommandLine(CommandLine line, Options options) {
        if (line.hasOption("help")) {
            HelpFormatter formatter = new HelpFormatter();
            printBanner();
            formatter.printHelp(NAME + " [options]", options);
            System.exit(0);
        }
        if (line.hasOption("version")) {
            printBanner();
            System.exit(0);
        }        
        if (line.hasOption("sname")) {
            this.sname = line.getOptionValue("sname");
        }
        if (line.hasOption("cookie")) {
            this.cookie = line.getOptionValue("cookie");
        }
        if (line.hasOption("peer")) {
            this.peer = line.getOptionValue("peer");
        }
        if (line.hasOption("ps")) {
            this.propf = line.getOptionValue("ps");
        }
	}
	
	private void printBanner() {
	    System.out.println(NAME + " version " + VERSION);
	}
	
	@SuppressWarnings("static-access")
    private Options getOptions() {
	    
	    Option help = new Option("help", "print this message");
	    Option version = new Option("version", "print the version information and exit");
	    Option sname = OptionBuilder.withArgName("sname")
                    .hasArg()
                    .withDescription("give short name of Java node")
                    .create("sname");
	    Option cookie = OptionBuilder.withArgName("cookie")
                    .hasArg()
                    .withDescription("Erlang cookie")
                    .create("cookie");
	    Option peer = OptionBuilder.withArgName("peer")
                    .hasArg()
                    .withDescription("give short name of Erlang node")
                    .create("peer");
	    Option propf = OptionBuilder.withArgName("properties")
        			.hasArg()
			        .withDescription("give path to properties file")
			        .create("ps");
	    
	    Options options = new Options();
	    options.addOption(help);
	    options.addOption(version);
	    options.addOption(sname);
	    options.addOption(cookie);
	    options.addOption(peer);
	    options.addOption(propf);
	    return options;
	}
	
	private void initProps(String path) throws IOException {
		Properties PROPERTIES = new Properties();
		try {
			FileInputStream stream = new FileInputStream(path);
			PROPERTIES.load(stream);
			stream.close();
		} catch (FileNotFoundException e) {
			System.out.println("ERROR: properties file not found at " + path);
		}
	}
	
	/**
	 * 
	 * @return
	 * @throws IllegalStateException
	 */
	public static JNode getJNode() throws IllegalStateException {
		if (NODE == null) {
			throw new IllegalStateException("JNode not initialized");
		}
		return NODE;
	}
	
	/**
	 * 
	 * @param key
	 * @param def
	 * @return
	 */
	public static String getProperty(String key, String def) {
		if (PROPERTIES == null) {
			throw new IllegalStateException("Properties not initialized");
		}
		return PROPERTIES.getProperty(key, def);		
	}
	
	/* MAIN */
	
	/**
	 * Main
	 */
    public static void main(String[] args) throws Exception {
        Main main = new Main(args);
        main.run();
    }
    
    

}
