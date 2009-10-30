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

import net.dushin.lethe.messaging.client.ui.controller.ChannelModel;
import net.dushin.lethe.messaging.client.ui.controller.LetheController;
import net.dushin.lethe.messaging.client.ui.controller.MessageChangeListener;
import net.dushin.lethe.messaging.client.ui.controller.Peer;
import net.dushin.lethe.messaging.client.ui.controller.PeerChangeListener;
import net.dushin.lethe.messaging.client.ui.controller.ReceivedMessage;
import net.dushin.lethe.messaging.common.collections.Pair;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;

public class ChannelPanel extends javax.swing.JPanel 
    implements MessageChangeListener, PeerChangeListener {

    private static final long serialVersionUID = -4313701965827987778L;
    private static final String NL = System.getProperty("line.separator");
    private final ChannelModel channel;
    
    private final javax.swing.JTextArea messageDisplayArea;
    private final javax.swing.JTextField sendMessageField;
    private final javax.swing.JCheckBox signCheckBox;
    private final javax.swing.JCheckBox encryptCheckBox;
    
    private boolean signMessages = true;
    private boolean encryptMessages = true;
    
    private final PeerPanel peerPanel; 
    
    private final TabbedChannelPanel parent;

    public 
    ChannelPanel(
        final LetheController controller,
        final String channelId,
        final TabbedChannelPanel parent
    ) {
        this.channel = controller.createChannel(channelId, this, this);
        this.parent = parent;
        
        setLayout(new java.awt.BorderLayout());
        
        this.messageDisplayArea = new javax.swing.JTextArea(5, 30);
        this.messageDisplayArea.setLineWrap(true);
        this.messageDisplayArea.setWrapStyleWord(true);
        this.messageDisplayArea.setEditable(false);

        final javax.swing.JPanel closeTabPanel = new javax.swing.JPanel();
        final javax.swing.JButton closeTabButton = new javax.swing.JButton("Close");
        closeTabButton.addActionListener(new CloseTabListener());
        closeTabPanel.setLayout(new java.awt.BorderLayout());
        closeTabPanel.add("East", closeTabButton);
        add("North", closeTabPanel);
        
        final javax.swing.JPanel sendCryptoPanel = new javax.swing.JPanel();
        
        
        signCheckBox = new javax.swing.JCheckBox("Sign");
        signCheckBox.setSelected(true);
        signCheckBox.addItemListener(new SignCheckBoxItemListener());
        encryptCheckBox = new javax.swing.JCheckBox("Encrypt");
        encryptCheckBox.setSelected(true);
        encryptCheckBox.addItemListener(new EncryptCheckBoxItemListener());
        
        this.peerPanel = new PeerPanel(this.channel);
        
        sendCryptoPanel.add(signCheckBox);
        sendCryptoPanel.add(encryptCheckBox);
        sendCryptoPanel.add(peerPanel);
        
        final javax.swing.JScrollPane messageScrollPane = 
            new javax.swing.JScrollPane(messageDisplayArea);
        messageScrollPane.setPreferredSize(new java.awt.Dimension(250, 250));

        final javax.swing.JSplitPane splitPane = new javax.swing.JSplitPane(
            javax.swing.JSplitPane.HORIZONTAL_SPLIT,
            sendCryptoPanel,
            messageScrollPane
        );
        splitPane.setOneTouchExpandable(true);
        // splitPane.setDividerLocation(150);

        add("Center", splitPane);

        final javax.swing.JPanel sendPanel = new javax.swing.JPanel();
        sendPanel.setLayout(new java.awt.BorderLayout());

        this.sendMessageField = new javax.swing.JTextField(20);
        this.sendMessageField.addKeyListener(new SendMessageKeyListener());
        sendPanel.add("Center", sendMessageField);
        
        final javax.swing.JButton sendButton = new javax.swing.JButton("Send");
        sendButton.addActionListener(new SendMessageListener());
        sendPanel.add("East", sendButton);
        
        add("South", sendPanel);
        
        // this.controller.registerMessageChangedListener(channel, this);
    }
    
    void
    closeTab() {
        this.channel.notifyHalt();
        parent.closeTab(this);
    }
    
    private void
    sendMessage() {
        final String sendText = sendMessageField.getText();
        if (sendText.length() > 0) {
            try {
                channel.sendMessage(
                    sendText, this.signMessages, this.encryptMessages
                );
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
            sendMessage();
        }        
    }
    
    private class CloseTabListener 
        implements java.awt.event.ActionListener {
        
        public void 
        actionPerformed(
            final java.awt.event.ActionEvent event
        ) {
            closeTab();
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
    
    private class SignCheckBoxItemListener implements java.awt.event.ItemListener {

        public void itemStateChanged(java.awt.event.ItemEvent e) {
            signMessages = e.getStateChange() == java.awt.event.ItemEvent.SELECTED;
        }
    }
    
    private class EncryptCheckBoxItemListener implements java.awt.event.ItemListener {

        public void itemStateChanged(java.awt.event.ItemEvent e) {
            encryptMessages = e.getStateChange() == java.awt.event.ItemEvent.SELECTED;
            peerPanel.setEnabled(encryptMessages);
        }
    }
    
    public void
    messageChanged(final java.util.List<ReceivedMessage> msgs) {
        final StringBuilder buf = new StringBuilder();
        // buf.append(this.messageDisplayArea.getText());
        for (ReceivedMessage msg : msgs) {
            buf.append("==========================\n");
            buf.append(displayStatus(msg));
            buf.append(' ');
            if (msg.getMessageEncrypted() && !msg.getMessageDecrypted()) {
                buf.append("...");
            } else {
                buf.append(displayPlaintextMessage(msg.getPlaintextMessage()));
            }
            buf.append(NL);
        }
        this.messageDisplayArea.setText(
            buf.toString()
        );
        //
        //
        //
        this.messageDisplayArea.selectAll();
        final int idx = this.messageDisplayArea.getSelectionEnd();
        this.messageDisplayArea.select(idx, idx);
    }
    
    private static String
    displayStatus(
        final ReceivedMessage msg
    ) {
        final StringBuilder buf = new StringBuilder();
        buf.append('[');
        if (msg.getMessageSigned() && msg.getMessageVerified()) { 
            buf.append("signed(" + msg.getSigner().getPinkyprint() + ")");
        } else if (msg.getMessageSigned()) {
            buf.append("signed but unverified");
        }
        if (msg.getMessageEncrypted()) {
            if (msg.getMessageSigned()) {
                buf.append('|');
            }
            buf.append("encrypted");
        }
        if (!msg.getMessageEncrypted() && !msg.getMessageSigned()) {
            buf.append("plaintext");
        }
        buf.append(']');
        return buf.toString();
    }
    
    private static String
    displayPlaintextMessage(
        final PlaintextMessage plaintext
    ) {
        final StringBuilder buf = new StringBuilder();
        buf.append(plaintext.getFrom());
        buf.append(": ");
        buf.append(plaintext.getData());
        buf.append('\n');
        return buf.toString();
    }

    public void peerChanged(final Pair<java.util.List<Peer>, java.util.List<Peer>> peers) {
        
        peerPanel.notifyChanged();
        
    }
}
