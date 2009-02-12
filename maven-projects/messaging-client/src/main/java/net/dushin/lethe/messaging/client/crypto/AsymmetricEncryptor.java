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
package net.dushin.lethe.messaging.client.crypto;

import net.dushin.lethe.messaging.interfaces.EncryptedMessage;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

public class AsymmetricEncryptor extends CryptorBase {

    public
    AsymmetricEncryptor(
        final java.security.PublicKey key
    ) {
        super(
            "RSA/ECB/NoPadding",
            javax.crypto.Cipher.ENCRYPT_MODE,
            key
        );
    }
    
    EncryptedMessage
    encrypt(final PlaintextMessage plaintext) {
        return encrypt(
            PlaintextMessage.class.getPackage(), 
            new javax.xml.bind.JAXBElement<PlaintextMessage>(
                net.dushin.lethe.messaging.interfaces.Constants.PLAINTEXT_MESSAGE_QNAME,
                PlaintextMessage.class,
                plaintext
            )
        );
    }
    
    EncryptedMessage
    encrypt(final SignedMessage signed) {
        return encrypt(SignedMessage.class.getPackage(), signed);
    }
    
    private EncryptedMessage
    encrypt(
        final Package pkg,
        final Object obj
    ) {
        try {
            final EncryptedMessage ret = new EncryptedMessage();
            final byte[] serialized = serialize(pkg, obj);
            final SymmetricEncryptor encryptor = new SymmetricEncryptor();
            final byte[] encrypted = encryptor.encrypt(serialized);
            ret.setEncryptedKey(encryptKey(encryptor.getSymmetricKey()));
            ret.setEncryptedData(encrypted);
            /*
            final java.io.ByteArrayOutputStream buf =
                new java.io.ByteArrayOutputStream();
            final int blockSize = this.cipher.getBlockSize();
            final int n = serialized.length/64;
            for (int i = 0; i < n;  ++i) {
                final byte[] encrypted = this.cipher.update(serialized, i * 64, 64);
                buf.write(encrypted, 0, encrypted.length);
            }
            int idx = n * 64;
            int len = serialized.length % 64;
            byte[] rem = new byte[64];
            System.arraycopy(serialized, idx, rem, 0, len);
            final byte[] encrypted = this.cipher.doFinal(rem, 0, 64);
            buf.write(encrypted, 0, encrypted.length);
            ret.setEncryptedData(
                buf.toByteArray()
            );
            */
            return ret;
        } catch (final Exception e) {
            throw new RuntimeException("Error attempting to encrypt", e);
        }
    }
    
    private byte[]
    encryptKey(
        final java.security.Key key
    ) {
        return encrypt(key.getEncoded());
    }
    
    private byte[]
    encrypt(
        final byte[] data
    ) {
        try {
            return this.cipher.doFinal(data);
        } catch (final Exception e) {
            throw new RuntimeException("Error encrypting data", e);
        }
    }
}
