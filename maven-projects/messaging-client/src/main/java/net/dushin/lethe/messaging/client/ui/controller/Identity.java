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

import net.dushin.lethe.messaging.client.crypto.Decryptor;
import net.dushin.lethe.messaging.client.crypto.KeyPairGenerator;
import net.dushin.lethe.messaging.client.crypto.Signer;

public class Identity extends Peer {

    public static final Identity ANONYMOUS = new Identity("anonymous", "anonymous", true, false);
    
    private final String password;
    private final java.security.KeyPair keyPair;
    private final Signer signer;
    private final Decryptor decryptor;
    
    private boolean signMessages;

    public
    Identity(
        final String name,
        final String password,
        final boolean signMessages,
        final boolean encryptToSelf
    ) {
        this(name, password, generateKeyPair(password), signMessages, encryptToSelf);
    }

    private
    Identity(
        final String name,
        final String password,
        final java.security.KeyPair keyPair,
        final boolean signMessages,
        final boolean encryptToSelf
    ) {
        super(name, keyPair.getPublic());
        this.password = password;
        this.keyPair = keyPair;
        this.signMessages = signMessages;
        this.signer = new Signer(this.keyPair.getPrivate());
        this.decryptor = new Decryptor(this.keyPair.getPrivate());
        this.setEncryptTo(encryptToSelf);
    }
    
    private static java.security.KeyPair
    generateKeyPair(final String password) {
        try {
            return new KeyPairGenerator(512).generateKeyPair(password);
        } catch (final Exception e) {
            throw new RuntimeException("Error generating key pair", e);
        }
    }
    
    public String
    getPassword() {
        return this.password;
    }
    
    public java.security.KeyPair
    getKeyPair() {
        return this.keyPair;
    }
    
    public Signer
    getSigner() {
        return this.signer;
    }
    
    public boolean
    getSignMessages() {
        return this.signMessages;
    }
    
    public void
    setSignMessages(
        final boolean signMessages
    ) {
        this.signMessages = signMessages;
    }
    
    public Decryptor
    getDecryptor() {
        return this.decryptor;
    }
}
