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

class RecipientTablePanel extends javax.swing.JPanel {

    private final LetheController controller;
    private final javax.swing.JTable recipientTable;
    
    RecipientTablePanel(
        final LetheController controller
    ) {
        this.controller = controller;
        
        this.setLayout(new java.awt.BorderLayout());
        
        //
        // TODO fix layout
        //
        java.awt.Button newButton = new java.awt.Button("Add...");
        newButton.addActionListener(new NewRecipientListener(this));
        this.add("South", newButton);
        
        this.recipientTable = createRecipientTable();
        final javax.swing.JScrollPane scrollPane = new javax.swing.JScrollPane(this.recipientTable);
        // this.recipientTable.setFillsViewportHeight(true);
        scrollPane.setBorder(
            javax.swing.BorderFactory.createCompoundBorder(
                javax.swing.BorderFactory.createCompoundBorder(
                    javax.swing.BorderFactory.createTitledBorder("Recipients"),
                    javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
                ),
                scrollPane.getBorder()
            )
        );        
        this.add("Center", scrollPane);
    }
    
    public void
    setEnabled(final boolean enabled) {
        super.setEnabled(enabled);
        this.recipientTable.setEnabled(enabled);
    }
    
    private static javax.swing.JTable
    createRecipientTable() {
        final String[] cols = {"Recipient", "Encrypt"};
        Object[][] data = {
            {"Mary", new Boolean(false)},
            {"Alison", new Boolean(true)},
            {"Kathy", new Boolean(false)},
            {"Sharon", new Boolean(true)},
            {"Philip", new Boolean(false)}
        };
        final javax.swing.JTable ret = new javax.swing.JTable(data, cols);
        return ret;
    }
    
    void
    addRecipient(
        final String serializedInput
    ) {
        
    }
    
    private class NewRecipientListener 
        implements java.awt.event.ActionListener {
        
        private final java.awt.Component parent;
        
        NewRecipientListener(final java.awt.Component parent) {
            this.parent = parent;
        }
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            final String serializedInput = javax.swing.JOptionPane.showInputDialog(
                parent, 
                "Recipient Data", 
                "Add Recipient...",
                javax.swing.JOptionPane.QUESTION_MESSAGE
            );
            if (serializedInput != null) {
                addRecipient(serializedInput);
            }
        }        
    }
    
}
