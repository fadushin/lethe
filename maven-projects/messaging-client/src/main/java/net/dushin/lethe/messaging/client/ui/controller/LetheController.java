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
package net.dushin.lethe.messaging.client.ui.controller;

import net.dushin.lethe.messaging.client.crypto.Encryptor;
import net.dushin.lethe.messaging.client.log.LogUtil;
import net.dushin.lethe.messaging.interfaces.Contents;
import net.dushin.lethe.messaging.interfaces.EncryptedMessage;
import net.dushin.lethe.messaging.interfaces.Message;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

/**
 * This class encapsulates the state of a Lethe client.  It holds
 * the identity information for the user (his or her key pair), together
 * with the information for all of the known peers of the user.
 *
 * The controller is used to send and receive messages through the
 * lethe system.  Each message is associated with a channel, each channel
 * is associated with a thread, which receives messages sent to the channel.
 */
public class LetheController {
    
    //
    // data
    //
    
    /**
     * The Logger instance to use for this type
     */
    private static final java.util.logging.Logger LOGGER =
        java.util.logging.Logger.getLogger(
            LetheController.class.getName()
        );
    
    /**
     * the connection to the server
     */
    private Connection connection = Connection.LOCALHOST;

    /**
     * The identity associated with this controller
     */
    private Identity identity = Identity.ANONYMOUS;
    
    /**
     * the collection of peers associated with this controller
     */
    private final java.util.List<Peer> peers =
        new java.util.ArrayList<Peer>();
    
    /**
     * a map from channel ids to threads.  Each thread is responsible for
     * receiving (in a polling manner) messages delivered to the designated
     * channel.
     */
    private final java.util.Map<String, MessageChangeThread> messageChangeThreadMap =
        new java.util.HashMap<String, MessageChangeThread>();
    
    //
    // Lifecycle
    //
    
    public
    LetheController() {
    }
    
    public
    LetheController(
        final Connection connection
    ) {
        this.connection = connection;
    }
    
    //
    // public operations
    //
    
    public Connection
    getConnection() {
        return this.connection;
    }
    
    public void
    setConnection(
        final Connection connection
    ) {
        this.connection = connection;
        synchronized (messageChangeThreadMap) {
            for (MessageChangeThread thread : messageChangeThreadMap.values()) {
                thread.notifyChange();
            }
        }
    }
    
    /**
     * @return      the Identity associated with this controller
     */
    public Identity
    getIdentity() {
        return this.identity;
    }
    
    /**
     * Set the identity associated with this controller.
     *
     * @param       identity
     *              the identity to set
     */
    public void
    setIdentity(
        final Identity identity
    ) {
        this.identity = identity;
        synchronized (messageChangeThreadMap) {
            for (MessageChangeThread thread : messageChangeThreadMap.values()) {
                thread.notifyChange();
            }
        }
    }
    
    /**
     * @return      the list of peers associated with this controller.
     */
    public java.util.List<Peer>
    getPeers() {
        return this.peers;
    }
    
    /**
     * Add a peer to the list of peers associated with this controller.
     *
     * @param       peer
     *              the Peer to add
     */
    public void
    addPeer(final Peer peer) {
        this.peers.add(peer);
        synchronized (messageChangeThreadMap) {
            for (MessageChangeThread thread : messageChangeThreadMap.values()) {
                thread.notifyChange();
            }
        }
    }
    
    /**
     * Remove the peers at the specified list of indices
     */
    public void
    removePeers(final int[] indices) {
        // TODO is this right?
        for (int index : indices) {
            this.peers.remove(index);
        }
        synchronized (messageChangeThreadMap) {
            for (MessageChangeThread thread : messageChangeThreadMap.values()) {
                thread.notifyChange();
            }
        }
    }
    
    /**
     * Send the specified message on the specified channel.  The specified
     * message will be signed and/or encrypted, based on the identity and peer
     * settings set on this controller.
     *
     * @param       channel
     *              the channel on which to send the message
     *
     * @param       The message (payload) to send.
     */
    public void
    sendMessage(
        final String channel,
        final String message
    ) {
        final Contents contents = new Contents();
        
        Object msg = null;
        
        final PlaintextMessage plaintext = new PlaintextMessage();
        plaintext.setFrom(this.identity.getName());
        plaintext.setData(message);
        //
        // message will be in plaintext unless told otherwise below
        //
        contents.setDescriptor(PlaintextMessage.class.getName());
        contents.setUuid(java.util.UUID.randomUUID().toString());
        msg = plaintext;
        //
        // If it's to be signed, do so
        //
        if (this.identity.getSignMessages()) {
            LogUtil.logInfo(
                LOGGER,
                "Message signing is selected; signing message..." 
            );
            contents.setDescriptor(SignedMessage.class.getName());
            msg = this.identity.getSigner().sign(plaintext);
        }
        //
        // And then encrypt, if there are any specified recipients
        //
        final java.util.List<java.security.PublicKey> recipients = getRecipients();
        if (recipients.size() > 0) {
            LogUtil.logInfo(
                LOGGER,
                "{0} recipient(s) selected; encrypting message...", 
                recipients.size()
            );
            contents.setDescriptor(EncryptedMessage.class.getName());
            msg = new Encryptor().encrypt(msg, recipients);
        }
        contents.setMsg(msg);
        try {
            LogUtil.logInfo(
                LOGGER,
                "Sending message to channel {0}...", 
                channel
            );
            this.connection.getProxy().postMessage(channel, contents);
            messageChangeThreadMap.get(channel).notifyChange();
        } catch (final Exception e) {
            LogUtil.logException(
                LOGGER,
                e,
                "An error occurred sending the message." 
            );
            e.printStackTrace();
        }
    }
    
    /**
     * Register a MessageChangeListener with this controller, using the channel
     * as the index.
     */
    public void
    registerMessageChangedListener(
        final String channel,
        final MessageChangeListener listener
    ) {
        synchronized (messageChangeThreadMap) {
            MessageChangeThread thread = messageChangeThreadMap.get(channel);
            if (thread == null) {
                thread = new MessageChangeThread(
                    channel, 
                    this,
                    listener
                );
                thread.start();
                messageChangeThreadMap.put(channel, thread);
            }
        }
    }
    
    /**
     * Remove the message change listener associated with the specified
     * channel.
     */
    public void
    removeMessageChangedListener(
        final String channel
    ) {
        synchronized (messageChangeThreadMap) {
            MessageChangeThread thread = messageChangeThreadMap.get(channel);
            if (thread != null) {
                thread.notifyHalt();
                messageChangeThreadMap.remove(channel);
            }
        }
    }
    
    java.util.List<ReceivedMessage>
    receiveMessages(
        final java.util.List<Message> messages
    ) {
        final java.util.List<ReceivedMessage> ret = new java.util.ArrayList<ReceivedMessage>();
        for (Message message : messages) {
            ret.add(receiveMessage(message));
        }
        return ret;
    }
    
    //
    // private operations
    //
    
    private ReceivedMessage
    receiveMessage(
        final Message message
    ) {
        final Contents contents = message.getMessage();
        String descriptor = contents.getDescriptor();
        if (descriptor.equals(PlaintextMessage.class.getName())) {
            final PlaintextMessage plaintext = (PlaintextMessage) contents.getMsg();
            return new ReceivedMessage(message, plaintext);
        } else if (descriptor.equals(SignedMessage.class.getName())) {
            final SignedMessage signed = (SignedMessage) contents.getMsg();
            try {
                final Object[] pair = verifyMessageSignedBy(signed);
                final Object plaintext = pair[0];
                final Peer signer = (Peer) pair[1];
                if (plaintext instanceof PlaintextMessage) {
                    return new ReceivedMessage(message, signed, signer, (PlaintextMessage) plaintext);
                } else {
                    throw new RuntimeException("Expected signed object to be plaintext");
                }
            } catch (final Exception e) {
                return new ReceivedMessage(message, signed);
            }
        } else if (descriptor.equals(EncryptedMessage.class.getName())) {
            final EncryptedMessage encrypted = (EncryptedMessage) contents.getMsg();
            Object obj = null; 
            try {
                obj = this.identity.getDecryptor().decrypt(encrypted);
            } catch (final Exception e) {
                return new ReceivedMessage(message, encrypted);
            }
            if (obj instanceof SignedMessage) {
                final SignedMessage signed = (SignedMessage) obj;
                Object plaintext = null;
                Peer signer = null;
                try {
                    final Object[] pair = verifyMessageSignedBy(signed);
                    plaintext = pair[0];
                    signer = (Peer) pair[1];
                } catch (final Exception e) {
                    return new ReceivedMessage(message, signed);
                }
                if (plaintext instanceof PlaintextMessage) {
                    return new ReceivedMessage(
                        message, encrypted, signed, signer, (PlaintextMessage) plaintext
                    );
                } else {
                    throw new RuntimeException("Expected signed object to be plaintext");
                }
            } else if (obj instanceof PlaintextMessage) {
                return new ReceivedMessage(message, encrypted, (PlaintextMessage) obj);
            }
        }
        throw new RuntimeException("Unsupported descriptor: " + descriptor);
    }

    private java.util.List<java.security.PublicKey>
    getRecipients() {
        final java.util.List<java.security.PublicKey> ret = 
            new java.util.ArrayList<java.security.PublicKey>();
        for (Peer peer : getPeers()) {
            if (peer.getEncryptTo()) {
                ret.add(peer.getPublicKey());
            }
        }
        if (ret.size() > 0 && this.identity.getEncryptTo()) {
            ret.add(this.identity.getPublicKey());
        }
        return ret;
    }
    
    private Object[]
    verifyMessageSignedBy(
        final SignedMessage signed
    ) {
        for (Peer peer : getIdentityAndPeers()) {
            try {
                final Object plaintext = peer.getVerifier().verify(signed);
                return new Object[] {plaintext, peer};
            } catch (final Exception e) {
                continue;
            }
        }
        throw new RuntimeException("No public key found");
    }
    
    private java.util.List<Peer>
    getIdentityAndPeers() {
        final java.util.List<Peer> ret = new java.util.ArrayList<Peer>();
        ret.add(this.identity);
        ret.addAll(this.peers);
        return ret;
    }
}
