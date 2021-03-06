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

import net.dushin.lethe.messaging.common.jaxb.JaxbSerialization;
import net.dushin.lethe.messaging.interfaces.EncryptedMessage;
import net.dushin.lethe.messaging.interfaces.Message;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

public class ReceivedMessage {

    private final boolean messageSigned;
    private final boolean messageVerified;
    
    private final boolean messageEncrypted;
    private final boolean messageDecrypted;
    
    private final Message rawMessage;
    private PlaintextMessage plaintext;
    private SignedMessage signed;
    private Peer signer;
    private EncryptedMessage encrypted;
    
    public
    ReceivedMessage(
        final Message rawMessage,
        final PlaintextMessage plaintext
    ) {
        this(rawMessage, false, false, false, false);
        this.plaintext = plaintext;
    }
    
    public
    ReceivedMessage(
        final Message rawMessage,
        final SignedMessage signed,
        final Peer signer,
        final PlaintextMessage plaintext
    ) {
        this(rawMessage, true, true, false, false);
        this.signed = signed;
        this.signer = signer;
        this.plaintext = plaintext;
    }
    
    public
    ReceivedMessage(
        final Message rawMessage,
        final SignedMessage signed
    ) {
        this(rawMessage, true, false, false, false);
        this.signed = signed;
        this.plaintext = parseSigned(signed);
    }
    
    public
    ReceivedMessage(
        final Message rawMessage,
        final EncryptedMessage encrypted
    ) {
        this(rawMessage, false, false, true, false);
        this.encrypted = encrypted;
    }
    
    public
    ReceivedMessage(
        final Message rawMessage,
        final EncryptedMessage encrypted,
        final SignedMessage signed,
        final Peer signer,
        final PlaintextMessage plaintext
    ) {
        this(rawMessage, true, true, true, true);
        this.encrypted = encrypted;
        this.signed = signed;
        this.signer = signer;
        this.plaintext = plaintext;
    }
    
    public
    ReceivedMessage(
        final Message rawMessage,
        final EncryptedMessage encrypted,
        final PlaintextMessage plaintext
    ) {
        this(rawMessage, false, false, true, true);
        this.encrypted = encrypted;
        this.plaintext = plaintext;
    }
    
    private
    ReceivedMessage(
        final Message rawMessage,
        final boolean messageSigned,
        final boolean messageVerified,
        final boolean messageEncrypted,
        final boolean messageDecrypted
    ) {
        this.rawMessage = rawMessage;
        this.messageSigned = messageSigned;
        this.messageVerified = messageVerified;
        this.messageEncrypted = messageEncrypted;
        this.messageDecrypted = messageDecrypted;
    }
    
    private static PlaintextMessage
    parseSigned(
        final SignedMessage signed
    ) {
        return JaxbSerialization.deserialize(
            PlaintextMessage.class.getPackage(), 
            signed.getSerializedMessage(), 
            PlaintextMessage.class);
    }
    
    public Message
    getRawMessage() {
        return this.rawMessage;
    }
    
    public boolean
    getMessageSigned() {
        return this.messageSigned;
    }
    
    public boolean
    getMessageVerified() {
        return this.messageVerified;
    }
    
    public boolean
    getMessageEncrypted() {
        return this.messageEncrypted;
    }
    
    public boolean
    getMessageDecrypted() {
        return this.messageDecrypted;
    }
    
    public PlaintextMessage
    getPlaintextMessage() {
        return this.plaintext;
    }
    
    public SignedMessage
    getSignedMessage() {
        return this.signed;
    }
    
    public Peer
    getSigner() {
        return this.signer;
    }
    
    public EncryptedMessage
    getEncryptedMessage() {
        return this.encrypted;
    }
}
