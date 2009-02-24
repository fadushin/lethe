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
import net.dushin.lethe.messaging.client.ui.controller.MessageChangeListener;
import net.dushin.lethe.messaging.interfaces.Contents;
import net.dushin.lethe.messaging.interfaces.Message;
import net.dushin.lethe.messaging.interfaces.MessageList;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;

public class MessagePanel extends javax.swing.JPanel 
    implements MessageChangeListener {

    private final LetheController controller;
    private final String channel;
    
    private final javax.swing.JTextArea messageDisplayArea;
    private final javax.swing.JTextField sendMessageField;

    public 
    MessagePanel(
        final LetheController controller,
        final String channel
    ) {
        this.controller = controller;
        this.channel = channel;
        
        setLayout(new java.awt.BorderLayout());
        
        this.messageDisplayArea = new javax.swing.JTextArea(5, 30);
        this.messageDisplayArea.setLineWrap(true);
        this.messageDisplayArea.setWrapStyleWord(true);
        this.messageDisplayArea.setEditable(false);

        final javax.swing.JScrollPane messageScrollPane = 
            new javax.swing.JScrollPane(messageDisplayArea);
        /*
        messageScrollPane.setVerticalScrollBarPolicy(
            javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS
        );
        */
        messageScrollPane.setPreferredSize(new java.awt.Dimension(250, 250));
        messageScrollPane.setBorder(
            javax.swing.BorderFactory.createCompoundBorder(
                javax.swing.BorderFactory.createCompoundBorder(
                    javax.swing.BorderFactory.createTitledBorder("Messages"),
                    javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5)
                ),
                messageScrollPane.getBorder()
            )
        );


        add("Center", messageScrollPane);
        

        final javax.swing.JPanel sendPanel = new javax.swing.JPanel();
        sendPanel.setLayout(new java.awt.FlowLayout());

        this.sendMessageField = new javax.swing.JTextField(20);
        this.sendMessageField.addKeyListener(new SendMessageKeyListener());
        sendPanel.add(sendMessageField);
        
        final javax.swing.JButton sendButton = new javax.swing.JButton("Send");
        sendButton.addActionListener(new SendMessageListener());
        sendPanel.add(sendButton);
        
        add("South", sendPanel);
        
        this.controller.registerMessageChangedListener(channel, this);
    }
    
    private void
    sendMessage() {
        final String sendText = sendMessageField.getText();
        if (sendText.length() > 0) {
            try {
                controller.sendMessage(channel, sendText);
            } catch (final Exception e) {
                e.printStackTrace();
                return;
            }
        }
        sendMessageField.setText("");
    }
    
    private class SendMessageListener 
        implements java.awt.event.ActionListener {
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            System.out.print(event);
            sendMessage();
        }        
    }
    
    private class SendMessageKeyListener
        implements java.awt.event.KeyListener {
        public void 
        keyTyped(
            final java.awt.event.KeyEvent e
        ) {
            // complete
        }

        public void 
        keyPressed(
            final java.awt.event.KeyEvent event
        ) {
            System.out.print(event);
            final int keyCode = event.getKeyCode();
            if (keyCode == java.awt.event.KeyEvent.VK_ENTER) {
                sendMessage();
            }
        }

        public void 
        keyReleased(
            final java.awt.event.KeyEvent e
        ) {
            // complete
        }
    }
    
    public void
    messageChanged(final MessageList msgs) {
        final StringBuilder buf = new StringBuilder();
        buf.append(this.messageDisplayArea.getText());
        for (Message msg : msgs.getItem()) {
            buf.append("=========\n");
            final Contents contents = msg.getMessage();
            if (contents.getDescriptor().equals(PlaintextMessage.class.getName())) {
                final PlaintextMessage plaintext = (PlaintextMessage) contents.getMsg();
                buf.append(plaintext.getFrom());
                buf.append(": ");
                buf.append(plaintext.getData());
                buf.append('\n');
            }
        }
        this.messageDisplayArea.setText(
            buf.toString()
        );
    }
}