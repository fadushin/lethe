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

import net.dushin.lethe.messaging.client.ui.controller.PeerSource;

class PeerPanel extends javax.swing.JPanel {

    private static final long serialVersionUID = -5090478463185083551L;
    private final PeerTableModel peerTableModel;
    private final javax.swing.JTable peerTable;
    
    PeerPanel(
        final PeerSource peerSource
    ) {
        this.setLayout(new java.awt.BorderLayout());
        
        //
        // TODO fix layout
        //

        this.peerTableModel = new PeerTableModel(peerSource);
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

    public void notifyChanged() {
        this.peerTableModel.fireTableDataChanged();
    }
}
