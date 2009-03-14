package net.dushin.lethe.messaging.client.ui;

import net.dushin.lethe.messaging.client.ui.components.LetheWindow;
import net.dushin.lethe.messaging.client.ui.components.SetIdentityDialog;
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
            final LetheWindow window = new LetheWindow(args);
            
            final SetIdentityDialog dlog = 
                new SetIdentityDialog(
                    window,
                    Identity.ANONYMOUS.getName(),
                    Identity.ANONYMOUS.getPassword()
                );
            dlog.setVisible(true);
            
            if (dlog.isOk()) {
                final String name = dlog.getName();
                final char[] passphrase = dlog.getPassphrase();
                final Identity identity =
                    new Identity(
                        name, 
                        new String(passphrase), 
                        Identity.ANONYMOUS.getSignMessages(),
                        true
                    );
                window.getController().setIdentity(identity);
            } else {
                return;
            }
            window.pack();
            window.setVisible(true);
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
