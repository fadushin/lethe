/**
 * Copyright (c) dushin.net
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of dushin.net nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package net.dushin.lethe.messaging.client.ui.components;

import net.dushin.lethe.messaging.client.ui.controller.LetheController;

public class
LetheWindow extends javax.swing.JFrame {

    private static final java.awt.event.ActionListener QUIT_LISTENER = new QuitActionListener();
    
    public
    LetheWindow() throws Exception {
        this("net.dushin.lethe");
    }
    
    public
    LetheWindow(final String title) throws Exception {
        this(title, 400, 300);
    }

    public 
    LetheWindow(
        final String title, 
        final int width, 
        final int height 
    ) throws Exception {
        super(title);

        // setBackground(java.awt.Color.GRAY);
        
        // Make the menu
        final javax.swing.JMenuItem quitMenuItem = new javax.swing.JMenuItem("Quit");
        quitMenuItem.addActionListener(QUIT_LISTENER);
        
        final javax.swing.JMenu fileMenu = new javax.swing.JMenu("File");
        fileMenu.add(quitMenuItem);
        
        final javax.swing.JMenuBar menuBar = new javax.swing.JMenuBar();
        menuBar.add(fileMenu);
        
        this.setJMenuBar(menuBar);
        
        setLayout(new java.awt.BorderLayout());
        final LetheController controller = new LetheController(
            new java.net.URL("http://localhost:18066/MessengerService/SOAPPort?wsdl")
        );
        LethePanel panel = new LethePanel(controller);
        add("Center", panel);
        pack();
        setVisible(true);
    }
    
    private static class
    QuitActionListener implements java.awt.event.ActionListener {
        
        public void
        actionPerformed(final java.awt.event.ActionEvent e) {
            System.exit(0);
        }
    }
}
