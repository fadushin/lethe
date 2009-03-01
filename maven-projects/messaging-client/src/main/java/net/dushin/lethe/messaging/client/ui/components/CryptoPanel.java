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

import net.dushin.lethe.messaging.client.ui.controller.Identity;
import net.dushin.lethe.messaging.client.ui.controller.LetheController;
import net.dushin.lethe.messaging.client.ui.controller.Peer;

class CryptoPanel extends javax.swing.JPanel {

    private final LetheController controller;
    
    private final javax.swing.JLabel idLabel = new javax.swing.JLabel();
    private final javax.swing.JCheckBox signBox = new javax.swing.JCheckBox(
            "Sign messages"
    );
    private final PeerTablePanel peerPanel;
    
    CryptoPanel(
        final LetheController controller
    ) {
        this.controller = controller;
        
        this.idLabel.setText("id: " + this.controller.getIdentity().getName());
        this.signBox.setSelected(
            this.controller.getIdentity().getSignMessages()
        );
        this.signBox.addItemListener(new SignBoxSelectionListener());
        
        this.setLayout(new java.awt.BorderLayout());

        final javax.swing.JButton idSetButton = new javax.swing.JButton("set...");
        idSetButton.addActionListener(new SetIdListener());
        final javax.swing.JButton idExportButton = new javax.swing.JButton("export...");
        idExportButton.addActionListener(new ExportIdListener());
        final javax.swing.JPanel idButtonPanel = new javax.swing.JPanel();
        idButtonPanel.add(idSetButton);
        idButtonPanel.add(idExportButton);
        
        final javax.swing.JPanel idPanel =
            new javax.swing.JPanel();
        idPanel.setLayout(new java.awt.BorderLayout());
        idPanel.add("North", this.idLabel);
        idPanel.add("Center", idButtonPanel);
        idPanel.add("South", this.signBox);
                
        this.add("North", idPanel);
        
        this.peerPanel =
            new PeerTablePanel(this.controller);
        this.add("Center", peerPanel);
    }
    
    private java.awt.Frame
    getFrame() {
        for (java.awt.Container container = this;; container = container.getParent()) {
            if (container == null) {
                return null;
            }
            if (container instanceof java.awt.Frame) {
                return (java.awt.Frame) container;
            }
        }
    }
    
    private void
    setNamePassphrase(
        final String name,
        final char[] passphrase
    ) {
        final Identity identity =
            new Identity(name, new String(passphrase), this.signBox.getSelectedObjects() != null);
        this.controller.setIdentity(
            identity
        );
        this.idLabel.setText("id: " + name);
        this.peerPanel.addPeer(new Peer(identity.getName(), identity.getKeyPair().getPublic()));
    }
    
    private void
    signBoxStateChanged() {
        final Object[] selected = this.signBox.getSelectedObjects();
        this.controller.getIdentity().setSignMessages(selected != null);
    }
    
    private class SetIdListener 
        implements java.awt.event.ActionListener {
        
        SetIdListener() {
        }
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            final SetIdentityDialog dlog = new SetIdentityDialog(getFrame());
            // dlog.setLocationRelativeTo(CryptoPanel.this);
            dlog.setVisible(true);
            
            if (dlog.isOk()) {
                final String name = dlog.getName();
                final char[] passphrase = dlog.getPassphrase();
                setNamePassphrase(name, passphrase);
            }
        }        
    }
    
    private class ExportIdListener 
        implements java.awt.event.ActionListener {
        
        ExportIdListener() {
        }
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            final ExportIdentityDialog dlog = new ExportIdentityDialog(
                getFrame(),
                CryptoPanel.this.controller.getIdentity()
            );
            // dlog.setLocationRelativeTo(CryptoPanel.this);
            dlog.setVisible(true);
        }        
    }
    
    private class SignBoxSelectionListener 
        implements java.awt.event.ItemListener {
        
        SignBoxSelectionListener() {
        }
        
        public void 
        itemStateChanged(
            final java.awt.event.ItemEvent event
        ) {
            signBoxStateChanged();
        }        
    }
}
