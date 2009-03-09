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
import net.dushin.lethe.messaging.client.ui.controller.Peer;

class PeerPanel extends javax.swing.JPanel {

    private final LetheController controller;
    private final PeerTableModel peerTableModel;
    private final javax.swing.JTable peerTable;
    
    PeerPanel(
        final LetheController controller
    ) {
        this.controller = controller;
        
        this.setLayout(new java.awt.BorderLayout());
        
        //
        // TODO fix layout
        //
        java.awt.Button addPeerButton = new java.awt.Button("Add...");
        addPeerButton.addActionListener(new AddPeerListener());
        java.awt.Button removePeerButton = new java.awt.Button("Remove");
        removePeerButton.addActionListener(new RemovePeerListener());

        final javax.swing.JPanel buttonPanel = new javax.swing.JPanel();
        buttonPanel.setLayout(new java.awt.FlowLayout());
        buttonPanel.add(addPeerButton);
        buttonPanel.add(removePeerButton);
        
        this.add("South", buttonPanel);
        
        this.peerTableModel = new PeerTableModel(this.controller);
        this.peerTable = new javax.swing.JTable(this.peerTableModel);
        final javax.swing.JScrollPane scrollPane = new javax.swing.JScrollPane(this.peerTable);
        // this.recipientTable.setFillsViewportHeight(true);
        this.setBorder(
            javax.swing.BorderFactory.createCompoundBorder(
                javax.swing.BorderFactory.createCompoundBorder(
                    javax.swing.BorderFactory.createTitledBorder("Peers"),
                    javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
                ),
                this.getBorder()
            )
        );        
        this.add("Center", scrollPane);
    }
    
    void
    addPeer(
        final String input
    ) {
        try {
            addPeer(new Peer(input));
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }
    
    void
    addPeer(
        final Peer peer
    ) {
        this.controller.addPeer(peer);            
        this.peerTableModel.fireTableDataChanged();
    }
    
    void
    removeSelectedPeers() {
        final int[] rows = this.peerTable.getSelectedRows();
        if (rows != null && rows.length > 0) {
            this.controller.removePeers(rows);
            this.peerTableModel.fireTableDataChanged();
        }
    }
    
    private class AddPeerListener 
        implements java.awt.event.ActionListener {
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            final AddPeerDialog dlog = new AddPeerDialog(SwingUtil.getFrame(PeerPanel.this));
            // dlog.setLocationRelativeTo(CryptoPanel.this);
            dlog.setVisible(true);
            
            if (dlog.isOk()) {
                final String input = dlog.getInput();
                addPeer(input);
            }
        }        
    }
    
    private class RemovePeerListener 
        implements java.awt.event.ActionListener {
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            removeSelectedPeers();
        }        
    }
    
}