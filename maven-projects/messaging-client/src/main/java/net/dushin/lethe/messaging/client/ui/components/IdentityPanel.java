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

class IdentityPanel extends javax.swing.JPanel {

    private static final long serialVersionUID = 8135340279515550327L;

    private final LetheController controller;

    // private final IdentityTableModel identityTableModel;
    // private final javax.swing.JTable identityTable;
    
    private final TitledLabelPanel nameLabel;
    
    private final TitledLabelPanel pinkyprintLabel;
    
    IdentityPanel(
        final LetheController controller
    ) {
        this.controller = controller;
        //
        // Build the identity panel
        //
        final javax.swing.JPanel idLabelPanel = new javax.swing.JPanel();
        nameLabel = new TitledLabelPanel("Name:", this.controller.getIdentity().getName());
        pinkyprintLabel = new TitledLabelPanel("Pinkyprint:", this.controller.getIdentity().getPinkyprint());
        idLabelPanel.setLayout(new java.awt.BorderLayout());
        idLabelPanel.add("North", nameLabel);
        idLabelPanel.add("South", pinkyprintLabel);
        
        //
        // Build the button panel and buttons
        //
        final javax.swing.JPanel idButtonPanel = new javax.swing.JPanel();
        final javax.swing.JButton idSetButton = new javax.swing.JButton("Generate...");
        idSetButton.addActionListener(new SetIdentityListener());
        final javax.swing.JButton idExportButton = new javax.swing.JButton("Export...");
        idExportButton.addActionListener(new ExportIdListener());
        idButtonPanel.add(idSetButton);
        idButtonPanel.add(idExportButton);

        this.setLayout(new java.awt.BorderLayout());
        this.setBorder(
            javax.swing.BorderFactory.createCompoundBorder(
                javax.swing.BorderFactory.createCompoundBorder(
                    javax.swing.BorderFactory.createTitledBorder("Identity"),
                    javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
                ),
                this.getBorder()
            )
        );
        this.add("Center", idLabelPanel);
        this.add("South", idButtonPanel);
    }
    
    void
    setIdentity(
        final Identity identity
    ) {
        this.controller.setIdentity(identity);
        this.nameLabel.getTextLabel().setText(identity.getName());
        this.pinkyprintLabel.getTextLabel().setText(identity.getPinkyprint());
    }
    
    private class SetIdentityListener 
        implements java.awt.event.ActionListener {
        
        SetIdentityListener() {
        }
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            final GenerateIdentityDialog dlog = 
                new GenerateIdentityDialog(
                    SwingUtil.getFrame(IdentityPanel.this),
                    IdentityPanel.this.controller.getIdentity().getName(),
                    IdentityPanel.this.controller.getIdentity().getPassword(),
                    IdentityPanel.this.controller.getIdentity().getKeySize()
                );
            dlog.setLocationRelativeTo(IdentityPanel.this);
            dlog.setVisible(true);
            
            if (dlog.isOk()) {
                final String name = dlog.getName();
                final char[] passphrase = dlog.getPassphrase();
                final Identity identity =
                    new Identity(
                        name, 
                        new String(passphrase),
                        dlog.getKeySize(),
                        true
                    );
                setIdentity(identity);
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
                SwingUtil.getFrame(IdentityPanel.this),
                IdentityPanel.this.controller.getIdentity()
            );
            dlog.setLocationRelativeTo(IdentityPanel.this);
            dlog.setVisible(true);
        }        
    }
}
