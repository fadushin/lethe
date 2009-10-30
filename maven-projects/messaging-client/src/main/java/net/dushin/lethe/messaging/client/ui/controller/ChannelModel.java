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
import net.dushin.lethe.messaging.common.collections.Pair;
import net.dushin.lethe.messaging.common.log.LogUtil;
import net.dushin.lethe.messaging.interfaces.Contents;
import net.dushin.lethe.messaging.interfaces.EncryptedMessage;
import net.dushin.lethe.messaging.interfaces.Message;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

public class ChannelModel implements PeerSource {

    //
    // data
    //
    
    /**
     * The Logger instance to use for this type
     */
    private static final java.util.logging.Logger LOGGER =
        java.util.logging.Logger.getLogger(
            ChannelModel.class.getName()
        );
    
    private final String channelId;

    private final ConnectionSource connectionSource;
    
    private final IdentitySource identitySource;
    
    /**
     * the collection of peers associated with this controller
     */
    private final java.util.Set<Peer> peers =
        new java.util.TreeSet<Peer>();
    
    private final ChannelUpdateThread updateThread;
    
    ChannelModel(
        final String channelId,
        final ConnectionSource connectionSource,
        final IdentitySource identitySource,
        final MessageChangeListener messageChangeListener,
        final PeerChangeListener peerChangeListener
    ) {
        this.channelId = channelId;
        this.connectionSource = connectionSource;
        this.identitySource = identitySource;
        this.updateThread = new ChannelUpdateThread(
            this,
            connectionSource,
            identitySource,
            messageChangeListener,
            peerChangeListener
        );
        this.updateThread.start();
    }
    
    //
    // public
    //
    
    public String
    getChannelId() {
        return this.channelId;
    }
    
    public synchronized java.util.List<Peer>
    getPeers() {
        return toList(this.peers);
    }
    
    public void
    sendMessage(
        final String message,
        final boolean signMessage,
        final boolean encryptMessage
    ) {
        final Contents contents = new Contents();
        
        Object msg = null;
        
        final PlaintextMessage plaintext = new PlaintextMessage();
        plaintext.setFrom(this.identitySource.getIdentity().getName());
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
        if (signMessage) {
            LogUtil.logInfo(
                LOGGER,
                "Message signing is selected; signing message..." 
            );
            contents.setDescriptor(SignedMessage.class.getName());
            msg = this.identitySource.getIdentity().getSigner().sign(plaintext);
        }
        //
        // And then encrypt, if there are any specified recipients
        //
        if (encryptMessage) {
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
        }
        contents.setMsg(msg);
        try {
            LogUtil.logInfo(
                LOGGER,
                "Sending message to channel {0}...", 
                channelId
            );
            this.connectionSource.getConnection().
                getChannelClientProxy(channelId).getProxy().postMessage(contents);
            updateThread.notifyChange();
        } catch (final Exception e) {
            LogUtil.logException(
                LOGGER,
                e,
                "An error occurred sending the message." 
            );
            e.printStackTrace();
        }
    }
    
    public void
    notifyChange() {
        this.updateThread.notifyChange();
    }
    
    public void
    notifyHalt() {
        this.updateThread.notifyHalt();
    }
    
    //
    // protected
    //
    
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
    // private
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
                obj = this.identitySource.getIdentity().getDecryptor().decrypt(encrypted);
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
        final Identity identity = this.identitySource.getIdentity();
        if (ret.size() > 0 && identity.getEncryptTo()) {
            ret.add(identity.getPublicKey());
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
    
    private synchronized java.util.List<Peer>
    getIdentityAndPeers() {
        final java.util.List<Peer> ret = new java.util.ArrayList<Peer>();
        ret.add(this.identitySource.getIdentity());
        ret.addAll(this.peers);
        return ret;
    }

    public synchronized Pair<java.util.List<Peer>, java.util.List<Peer>> 
    reconcilePeers(
        final java.util.List<net.dushin.lethe.messaging.interfaces.Peer> structs
    ) {
        final java.util.Set<Peer> reportedPeers = new java.util.TreeSet<Peer>();
        for (final net.dushin.lethe.messaging.interfaces.Peer struct : structs) {
            reportedPeers.add(new Peer(struct.getEncoded()));
        }
        final java.util.Set<Peer> joined = relativeComplement(reportedPeers, peers);
        for (final Peer p : joined) {
            peers.add(p);
        }
        final java.util.Set<Peer> left = relativeComplement(peers, reportedPeers);
        for (final Peer p : left) {
            peers.remove(p);
        }
        return new Pair<java.util.List<Peer>, java.util.List<Peer>>(
            toList(joined), toList(left)
        );
    }
    
    private java.util.List<Peer> toList(java.util.Collection<Peer> col) {
        return java.util.Collections.list(java.util.Collections.enumeration(col));
    }

    private static <T> java.util.Set<T>
    relativeComplement(
        final java.util.Set<T> a,
        final java.util.Set<T> b
    ) {
        final java.util.Set<T> ret = new java.util.TreeSet<T>();
        for (final T t : a) {
            if (!b.contains(t)) {
                ret.add(t);
            }
        }
        return ret;
    }
}
