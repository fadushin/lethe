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
import net.dushin.lethe.messaging.client.ws.MessengerClientProxy;
import net.dushin.lethe.messaging.interfaces.Contents;
import net.dushin.lethe.messaging.interfaces.EncryptedMessage;
import net.dushin.lethe.messaging.interfaces.Message;
import net.dushin.lethe.messaging.interfaces.Messenger;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

public class LetheController {

    final java.net.URL wsdlLoc;

    MessengerClientProxy proxy;
    Identity identity;
    
    private final java.util.Map<String, MessageChangeThread> messageChangeThreadMap =
        new java.util.HashMap<String, MessageChangeThread>();
    
    private final java.util.List<Peer> peers =
        new java.util.ArrayList<Peer>();
    
    public
    LetheController(
        final java.net.URL wsdlLoc
    ) throws Exception {
        this(wsdlLoc, Identity.ANONYMOUS);
    }
    
    public
    LetheController(
        final java.net.URL wsdlLoc,
        final Identity identity
    ) throws Exception {
        this.wsdlLoc = wsdlLoc;
        this.identity = identity;
        // this.peers.add(new Peer(this.identity.getName(), this.identity.getKeyPair().getPublic()));
    }
    
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
        msg = plaintext;
        //
        // If it's to be signed, do so
        //
        if (this.identity.getSignMessages()) {
            contents.setDescriptor(SignedMessage.class.getName());
            msg = this.identity.getSigner().sign(plaintext);
        }
        //
        // And then encrypt, if there are any specified recipients
        //
        final java.util.List<java.security.PublicKey> recipients = getRecipients();
        if (recipients.size() > 0) {
            contents.setDescriptor(EncryptedMessage.class.getName());
            msg = new Encryptor().encrypt(msg, recipients);
        }
        contents.setMsg(msg);
        try {
            getProxy().postMessage(channel, contents);
        } catch (final Exception e) {
            // log
            e.printStackTrace();
        }
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
    
    Messenger
    getProxy() {
        synchronized (this) {
            try {
                if (this.proxy == null) {
                    this.proxy = new MessengerClientProxy(this.wsdlLoc);
                }
                return this.proxy.getProxy();
            } catch (final Exception e) {
                // log it?
                throw new RuntimeException("Unable to resolve proxy", e);
            }
        }
    }
    
    public void
    addPeer(final Peer peer) {
        this.peers.add(peer);
        synchronized (messageChangeThreadMap) {
            for (MessageChangeThread thread : messageChangeThreadMap.values()) {
                thread.notifyChange();
            }
        }
    }
    
    public void
    removePeers(final int[] indices) {
        for (int index : indices) {
            this.peers.remove(index);
        }
        synchronized (messageChangeThreadMap) {
            for (MessageChangeThread thread : messageChangeThreadMap.values()) {
                thread.notifyChange();
            }
        }
    }
    
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
    
    ReceivedMessage
    receiveMessage(
        final Message message
    ) {
        final Contents contents = message.getMessage();
        String descriptor = contents.getDescriptor();
        if (descriptor.equals(PlaintextMessage.class.getName())) {
            final PlaintextMessage plaintext = (PlaintextMessage) contents.getMsg();
            return new ReceivedMessage(plaintext);
        } else if (descriptor.equals(SignedMessage.class.getName())) {
            final SignedMessage signed = (SignedMessage) contents.getMsg();
            try {
                final Object[] pair = verifyMessageSignedBy(signed);
                final Object plaintext = pair[0];
                final Peer signer = (Peer) pair[1];
                if (plaintext instanceof PlaintextMessage) {
                    return new ReceivedMessage(signed, signer, (PlaintextMessage) plaintext);
                } else {
                    throw new RuntimeException("Expected signed object to be plaintext");
                }
            } catch (final Exception e) {
                return new ReceivedMessage(signed);
            }
        } else if (descriptor.equals(EncryptedMessage.class.getName())) {
            final EncryptedMessage encrypted = (EncryptedMessage) contents.getMsg();
            Object obj = null; 
            try {
                obj = this.identity.getDecryptor().decrypt(encrypted);
            } catch (final Exception e) {
                return new ReceivedMessage(encrypted);
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
                    return new ReceivedMessage(signed);
                }
                if (plaintext instanceof PlaintextMessage) {
                    return new ReceivedMessage(
                        encrypted, signed, signer, (PlaintextMessage) plaintext
                    );
                } else {
                    throw new RuntimeException("Expected signed object to be plaintext");
                }
            } else if (obj instanceof PlaintextMessage) {
                return new ReceivedMessage(encrypted, (PlaintextMessage) obj);
            }
        }
        throw new RuntimeException("Unsupported descriptor: " + descriptor);
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
    
    public Identity
    getIdentity() {
        return this.identity;
    }
    
    public java.util.List<Peer>
    getPeers() {
        return this.peers;
    }
}
