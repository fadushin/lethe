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

class TabbedMessagePanel extends javax.swing.JPanel {

    private final LetheController controller;
    private final javax.swing.JTabbedPane tabs;
    
    TabbedMessagePanel(
        final LetheController controller
    ) {
        this.controller = controller;
        
        this.setLayout(new java.awt.BorderLayout());
        
        final javax.swing.JPanel buttonPanel = new javax.swing.JPanel();
        buttonPanel.setLayout(new java.awt.BorderLayout());
        final java.awt.Button newButton = new java.awt.Button("Add...");
        newButton.addActionListener(new NewChannelListener(this));
        buttonPanel.add("West", newButton);
        this.add("North", buttonPanel);
        
        this.tabs = new javax.swing.JTabbedPane();
        this.add("Center", this.tabs);

        this.setBorder(
            javax.swing.BorderFactory.createCompoundBorder(
                javax.swing.BorderFactory.createCompoundBorder(
                    javax.swing.BorderFactory.createTitledBorder("Channels"),
                    javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
                ),
                this.getBorder()
            )
        );
    }
    
    void
    createTabbedPane(
        final String channel
    ) {
        final MessagePanel panel = new MessagePanel(controller, channel, this);
        final int index = this.tabs.getComponentCount();
        this.tabs.addTab(channel, panel);
        /*
             REQUIRES 1.6 !!
        this.tabs.setTabComponentAt(
            index, 
            new CloseableTabComponent(this.tabs)
        );
        */
    }
    
    void
    closeTab(
        final MessagePanel panel
    ) {
        final int index = this.tabs.indexOfComponent(panel);
        if (index != -1) {
            this.tabs.remove(index);
        }
    }
    
    private class NewChannelListener 
        implements java.awt.event.ActionListener {
        
        private final java.awt.Component parent;
        
        NewChannelListener(final java.awt.Component parent) {
            this.parent = parent;
        }
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            final String channel = javax.swing.JOptionPane.showInputDialog(
                parent, 
                "Channel Name", 
                "New Channel...",
                javax.swing.JOptionPane.QUESTION_MESSAGE
            );
            if (channel != null) {
                createTabbedPane(channel);
            }
        }        
    }
    
}
