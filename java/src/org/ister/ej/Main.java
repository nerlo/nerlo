package org.ister.ej;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

import org.apache.commons.cli.*;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

/**
 * Main class.
 *
 * @author ingo
 */
public class Main {

	private final String[] args;
	private final String pwd = System.getProperty("user.dir");
	
	private String sname  = "jnode";
	private String cookie = "123456";
	private String peer   = "shell";
	private String handlerClass = "org.ister.ej.SimpleMsgHandler";
	private String propf  = pwd + "/properties";
	
	private static Node NODE = null;
	private static Properties PROPERTIES = null;
	
	private static final String VERSION = "0.0.1-alpha";
	private static final String NAME = "nerlo";
	
	private static final Logger LOG = Logger.getRootLogger();
	
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
	    LOG.info("---- Main initialized");
        NODE = Node.getInstance(sname, peer, PROPERTIES);
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
        if (line.hasOption("handlerClass")) {
            this.handlerClass = line.getOptionValue("handlerClass");
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
	    Option hdClass = OptionBuilder.withArgName("handlerClass")
					.hasArg()
			        .withDescription("give class of message handler")
			        .create("handlerClass");
	    
	    Options options = new Options();
	    options.addOption(help);
	    options.addOption(version);
	    options.addOption(sname);
	    options.addOption(cookie);
	    options.addOption(peer);
	    options.addOption(propf);
	    options.addOption(hdClass);
	    return options;
	}
	
	private void initProps(String path) throws IOException {
		PROPERTIES = new Properties();
		try {
			FileInputStream stream = new FileInputStream(path);
			PROPERTIES.load(stream);
			PropertyConfigurator.configure(PROPERTIES);
			stream.close();
		} catch (FileNotFoundException e) {
			System.out.println("ERROR: properties file not found at " + path);
		} finally {
			if (PROPERTIES.getProperty("ej.cookie") == null) {
				PROPERTIES.setProperty("ej.cookie", this.cookie);
			}
			if (PROPERTIES.getProperty("ej.msgHandler") == null) {
				PROPERTIES.setProperty("ej.msgHandler", this.handlerClass);
			}
		}
	}
	
	/**
	 * 
	 * @return
	 * @throws IllegalStateException
	 */
	public static Node getJNode() throws IllegalStateException {
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
	
    
    /**
     * Get the application logger.
     * 
     * @return
     */
    public static Logger getLogger() {
    	return LOG;
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
