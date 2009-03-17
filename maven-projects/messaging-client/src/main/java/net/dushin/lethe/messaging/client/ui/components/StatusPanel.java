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

import net.dushin.lethe.messaging.client.ui.controller.Connection;
import net.dushin.lethe.messaging.client.ui.controller.LetheController;

class StatusPanel extends javax.swing.JPanel {

    private final LetheController controller;
    
    private final javax.swing.JLabel hostLabel;
    private final javax.swing.JLabel portLabel;

    StatusPanel(
        final LetheController controller
    ) {
        this.controller = controller;
        
        this.setBorder(
            javax.swing.BorderFactory.createCompoundBorder(
                javax.swing.BorderFactory.createCompoundBorder(
                    javax.swing.BorderFactory.createTitledBorder("Status"),
                    javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
                ),
                this.getBorder()
            )
        );
        
        this.hostLabel = 
            new javax.swing.JLabel(getHostText());
        this.portLabel = 
            new javax.swing.JLabel(getPortText());
        
        final javax.swing.JButton setConnectionButton = 
            new javax.swing.JButton("Settings...");
        setConnectionButton.addActionListener(new SetConnectionListener());
        final javax.swing.JButton helpButton = 
            new javax.swing.JButton("Help...");
        //
        // add them to this panel
        //
        // this.setLayout(new java.awt.BorderLayout());
        // this.add("Center", splitPane);
        this.add(this.hostLabel);
        this.add(this.portLabel);
        this.add(setConnectionButton);
        this.add(helpButton);

    }
    
    private String
    getHostText() {
        return "Host:" + this.controller.getConnection().getHost();
    }
    
    private String
    getPortText() {
        return "Port:" + this.controller.getConnection().getPort();
    }
    
    private void
    setConnection(
        final Connection connection
    ) {
        this.controller.setConnection(connection);
        this.hostLabel.setText(getHostText());
        this.portLabel.setText(getPortText());
    }
    
    private class SetConnectionListener 
        implements java.awt.event.ActionListener {
        
        SetConnectionListener() {
        }
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            final SetConnectionDialog dlog = 
                new SetConnectionDialog(
                    SwingUtil.getFrame(StatusPanel.this),
                    StatusPanel.this.controller.getConnection().getHost(),
                    Short.toString(StatusPanel.this.controller.getConnection().getPort())
                );
            dlog.setLocationRelativeTo(StatusPanel.this);
            dlog.setVisible(true);
            
            if (dlog.isOk()) {
                final String host = dlog.getHost();
                final String port = dlog.getPort();
                final Connection connection =
                    new Connection(
                        host, 
                        Short.parseShort(port)
                    );
                setConnection(connection);
            }
        }        
    }
}
