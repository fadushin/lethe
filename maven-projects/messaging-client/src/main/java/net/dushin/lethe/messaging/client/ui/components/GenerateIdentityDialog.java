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

public class GenerateIdentityDialog extends javax.swing.JDialog {
    
    private static final Integer[] KEY_SIZES = {
        512,
        1024,
        2048,
        4096
    };
    
    private final javax.swing.JTextField nameField =
        new javax.swing.JTextField(20);
    private final javax.swing.JPasswordField passphraseField =
        new javax.swing.JPasswordField(20);
    private final javax.swing.JComboBox keySizeComboBox = 
        new javax.swing.JComboBox(KEY_SIZES);

    private String name;
    private char[] passphrase;
    private boolean ok;
    
    public 
    GenerateIdentityDialog(
        final java.awt.Frame owner,
        final String name,
        final String password,
        final int keySize
    ) {
        super(owner, true);

        setTitle("Generate Identity...");
        
        this.nameField.setText(name);
        this.passphraseField.setText(password);
        
        final javax.swing.JPanel namePanel = new javax.swing.JPanel();
        namePanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.TRAILING));            
        final javax.swing.JLabel nameLabel = new javax.swing.JLabel("Name: ");
        nameLabel.setLabelFor(this.nameField);
        namePanel.add(nameLabel);
        namePanel.add(this.nameField);
        
        final javax.swing.JPanel passphrasePanel = new javax.swing.JPanel();
        passphrasePanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.TRAILING));            
        final javax.swing.JLabel passphraseLabel = new javax.swing.JLabel("Passphrase: ");
        passphraseLabel.setLabelFor(this.passphraseField);
        passphrasePanel.add(passphraseLabel);
        passphrasePanel.add(this.passphraseField);
        
        final javax.swing.JPanel keySizePanel = new javax.swing.JPanel();
        keySizePanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.TRAILING));
        final javax.swing.JLabel keySizeLabel = new javax.swing.JLabel("Key Size: ");
        keySizeLabel.setLabelFor(this.keySizeComboBox);
        keySizePanel.add(keySizeLabel);
        keySizePanel.add(keySizeComboBox);
        this.keySizeComboBox.setSelectedItem(new Integer(keySize));

        final javax.swing.JPanel contentPanel = new javax.swing.JPanel();
        contentPanel.setLayout(new java.awt.BorderLayout());
        contentPanel.add("North", namePanel);
        contentPanel.add("Center", passphrasePanel);
        // contentPanel.add("South", keySizePanel);
        
        contentPanel.setBorder(
            javax.swing.BorderFactory.createCompoundBorder(
                javax.swing.BorderFactory.createCompoundBorder(
                    javax.swing.BorderFactory.createTitledBorder("Generate Identity"),
                    javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
                ),
                contentPanel.getBorder()
            )
        );
        
        final javax.swing.JButton okButton = new javax.swing.JButton("OK");
        okButton.addActionListener(new OkButtonListener());
        getRootPane().setDefaultButton(okButton);
        final javax.swing.JButton cancelButton = new javax.swing.JButton("Cancel");
        cancelButton.addActionListener(new CancelButtonListener());
        
        final javax.swing.JPanel buttonPanel = new javax.swing.JPanel();
        buttonPanel.setLayout(new java.awt.FlowLayout());
        buttonPanel.add(okButton);
        buttonPanel.add(cancelButton);
        
        this.setLayout(new java.awt.BorderLayout());
        this.add("Center", contentPanel);
        this.add("South", buttonPanel);

        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        
        pack();
    }
    
    public boolean
    isOk() {
        return this.ok;
    }
    
    public String
    getName() {
        return this.name;
    }
    
    public char[]
    getPassphrase() {
        return this.passphrase;
    }
    
    public int
    getKeySize() {
        return 512;
        // return (Integer) this.keySizeComboBox.getSelectedItem();
    }

    /** This method clears the dialog and hides it. */
    private void 
    clearAndHide() {
        this.nameField.setText("");
        this.passphraseField.setText("");
        setVisible(false);
    }
    
    private void
    ok() {
        this.name = this.nameField.getText();
        this.passphrase = this.passphraseField.getPassword();
        this.ok = true;
    }
    
    private void
    cancel() {
        this.name = null;
        this.passphrase = null;
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
