package net.dushin.lethe.messaging.client.ui;

import net.dushin.lethe.messaging.client.ui.components.GenerateIdentityDialog;
import net.dushin.lethe.messaging.client.ui.components.LetheWindow;
import net.dushin.lethe.messaging.client.ui.controller.Connection;
import net.dushin.lethe.messaging.client.ui.controller.Identity;

public final class Main {

    private static final String FLAG = "-";
    private static final String FLAG_HOST = FLAG + LetheWindow.TAG_HOST;
    private static final String FLAG_PORT = FLAG + LetheWindow.TAG_PORT;
        
    private static final java.util.Map<String, Object> DEFAULT_CONFIG = 
        new java.util.HashMap<String, Object>();
    static {
        DEFAULT_CONFIG.put(LetheWindow.TAG_HOST, Connection.DEFAULT_HOST);
        DEFAULT_CONFIG.put(LetheWindow.TAG_PORT, Connection.DEFAULT_PORT);
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
            if (i < n - 1 && argv[i].equals(FLAG_HOST)) {
                ret.put(LetheWindow.TAG_HOST, argv[++i]);
            }
            if (i < n - 1 && argv[i].equals(FLAG_PORT)) {
                ret.put(LetheWindow.TAG_PORT, Short.valueOf(argv[++i]));
            }
            if (argv[i].equals("-help")) {
                System.out.println(syntax());
                System.exit(0);
            }
        }
        return ret;
    }
    
    private static void 
    startUI(
        final java.util.Map<String, Object> args
    ) {
        try {
            final java.awt.Frame window = new java.awt.Frame();
            window.setLocationRelativeTo(null);
            final GenerateIdentityDialog dlog = 
                new GenerateIdentityDialog(
                    window,
                    "",
                    "",
                    512
                );
            dlog.pack();
            dlog.setLocationRelativeTo(window);
            dlog.setVisible(true);
            
            Identity identity = Identity.ANONYMOUS;
            if (dlog.isOk()) {
                final String name = dlog.getName();
                final char[] passphrase = dlog.getPassphrase();
                identity = new Identity(
                    name, 
                    new String(passphrase), 
                    dlog.getKeySize(),
                    // Identity.ANONYMOUS.getSignMessages(),
                    true
                );
                dlog.pack();
            } else {
                System.exit(1);
            }

            final LetheWindow letheWindow = new LetheWindow(args);
            letheWindow.setIdentity(identity);
            letheWindow.pack();
            letheWindow.setSize(900, 700);
            letheWindow.setLocationRelativeTo(null);
            letheWindow.setVisible(true);
            
            
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }
    
    private static String
    syntax() {
        return "Syntax: java "
            + Main.class.getName() + ' ' 
            + '[' + FLAG_HOST + " <host> (default: " + Connection.DEFAULT_HOST + ')' + ' '
            + '[' + FLAG_PORT + " <port> (default: " + Connection.DEFAULT_PORT + ')' + ' '
            + "[-help]";
    }
}
