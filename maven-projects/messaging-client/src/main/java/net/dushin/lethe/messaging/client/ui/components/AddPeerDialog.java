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

class AddPeerDialog extends javax.swing.JDialog {
    
    private final javax.swing.JTextArea inputTextArea =
        new javax.swing.JTextArea(20, 40);

    private String input;
    private boolean ok;
    
    public 
    AddPeerDialog(
        final java.awt.Frame owner 
    ) {
        super(owner, true);

        setTitle("Add Peer...");
        
        final javax.swing.JButton okButton = new javax.swing.JButton("ok");
        okButton.addActionListener(new OkButtonListener());
        final javax.swing.JButton cancelButton = new javax.swing.JButton("cancel");
        cancelButton.addActionListener(new CancelButtonListener());
        
        final javax.swing.JPanel buttonPanel = new javax.swing.JPanel();
        buttonPanel.setLayout(new java.awt.GridLayout(0, 1));
        buttonPanel.add(okButton);
        buttonPanel.add(cancelButton);
        
        this.setLayout(new java.awt.BorderLayout());
        this.add("Center", inputTextArea);
        this.add("South", buttonPanel);

        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        
        pack();
    }
    
    public boolean
    isOk() {
        return this.ok;
    }
    
    public String
    getInput() {
        return this.input;
    }

    /** This method clears the dialog and hides it. */
    private void 
    clearAndHide() {
        this.inputTextArea.setText("");
        setVisible(false);
    }
    
    private void
    ok() {
        this.input = this.inputTextArea.getText();
        this.ok = true;
    }
    
    private void
    cancel() {
        this.input = null;
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
    
    private class CancelButtonListener 
        implements java.awt.event.ActionListener {
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            cancel();
            clearAndHide();
        }        
    }
}
