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

import net.dushin.lethe.messaging.client.crypto.KeyHelper;
import net.dushin.lethe.messaging.client.ui.controller.Identity;

class ExportIdentityDialog extends javax.swing.JDialog {
    
    private final javax.swing.JTextArea exportTextArea =
        new javax.swing.JTextArea(20, 40);

    private boolean ok;
    
    public 
    ExportIdentityDialog(
        final java.awt.Frame owner,
        final Identity identity
    ) {
        super(owner, true);

        setTitle("Export...");
        
        this.exportTextArea.setText(
            KeyHelper.toString(
                identity.getName(), 
                identity.getKeyPair().getPublic()
            )
        );
        this.exportTextArea.setEditable(false);
        this.exportTextArea.selectAll();
        
        final javax.swing.JButton okButton = new javax.swing.JButton("OK");
        getRootPane().setDefaultButton(okButton);
        okButton.addActionListener(new OkButtonListener());
        
        final javax.swing.JPanel buttonPanel = new javax.swing.JPanel();
        buttonPanel.setLayout(new java.awt.FlowLayout());
        buttonPanel.add(okButton);
        
        final javax.swing.JPanel pane = new javax.swing.JPanel(new java.awt.BorderLayout());
        pane.add("Center", this.exportTextArea);
        pane.setBorder(
            javax.swing.BorderFactory.createCompoundBorder(
                javax.swing.BorderFactory.createCompoundBorder(
                    javax.swing.BorderFactory.createTitledBorder("Export Peer"),
                    javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
                ),
                pane.getBorder()
            )
        );

        this.setLayout(new java.awt.BorderLayout());
        this.add("Center", pane);
        this.add("South", buttonPanel);

        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        
        pack();
    }
    
    public boolean
    isOk() {
        return this.ok;
    }
    

    /** This method clears the dialog and hides it. */
    private void 
    clearAndHide() {
        this.exportTextArea.setText("");
        setVisible(false);
    }
    
    private void
    ok() {
        this.ok = true;
    }
    
    private void
    cancel() {
        this.ok = false;
    }
    
    private class OkButtonListener 
        implements java.awt.event.ActionListener {
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            ok();
            clearAndHide();
        }        
    }
}
