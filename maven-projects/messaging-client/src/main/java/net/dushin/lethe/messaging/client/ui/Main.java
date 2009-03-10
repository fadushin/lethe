package net.dushin.lethe.messaging.client.ui;

import net.dushin.lethe.messaging.client.ui.components.LetheWindow;

public final class Main {

    private static final String FLAG = "-";
    private static final String FLAG_HOSTNAME = FLAG + LetheWindow.TAG_HOSTNAME;
    private static final String FLAG_PORT = FLAG + LetheWindow.TAG_PORT;
    
    private static final String DEFAULT_HOSTNAME = "localhost";
    private static final Short DEFAULT_PORT = 8080;
    
    private static final java.util.Map<String, Object> DEFAULT_CONFIG = 
        new java.util.HashMap<String, Object>();
    static {
        DEFAULT_CONFIG.put(LetheWindow.TAG_HOSTNAME, DEFAULT_HOSTNAME);
        DEFAULT_CONFIG.put(LetheWindow.TAG_PORT, DEFAULT_PORT);
    }
    
    private Main() {
        // complete
    }
    
    public static void
    main(
        final String[] argv
    ) throws Exception {
        javax.swing.SwingUtilities.invokeLater(
            new Runner(parseArgv(argv))
        );
    }
    
    private static class Runner implements Runnable {
        
        private final java.util.Map<String, Object> args;
        
        Runner(
            final java.util.Map<String, Object> args
        ) {
            this.args = args;
        }
        
        public void run() {
            startUI(args);
        }
    }
    
    private static java.util.Map<String, Object>
    parseArgv(
        final String[] argv
    ) {
        final int n = argv.length;
        final java.util.Map<String, Object> ret = 
            new java.util.HashMap<String, Object>(DEFAULT_CONFIG);
        for (int i = 0;  i < n;  ++i) {
            if (i < n - 1 && argv[i].equals(FLAG_HOSTNAME)) {
                ret.put(LetheWindow.TAG_HOSTNAME, argv[++i]);
            }
            if (i < n - 1 && argv[i].equals(FLAG_PORT)) {
                ret.put(LetheWindow.TAG_PORT, Short.valueOf(argv[++i]));
            }
            if (argv[i].equals("-help")) {
                System.out.println(syntax());
            }
        }
        return ret;
    }
    
    private static void 
    startUI(
        final java.util.Map<String, Object> args
    ) {
        try {
            new LetheWindow(args);
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }
    
    private static String
    syntax() {
        return "Syntax: java "
            + Main.class.getName() + ' ' 
            + '[' + FLAG_HOSTNAME + " <hostname> (default: " + DEFAULT_HOSTNAME + ')' + ' '
            + '[' + FLAG_PORT + " <port> (default: " + DEFAULT_PORT + ')' + ' '
            + "[-help]";
    }
}
