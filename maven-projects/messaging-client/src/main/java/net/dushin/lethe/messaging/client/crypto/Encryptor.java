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

import net.dushin.lethe.messaging.client.debug.HexDump;
import net.dushin.lethe.messaging.interfaces.EncryptedKey;
import net.dushin.lethe.messaging.interfaces.EncryptedKeyList;
import net.dushin.lethe.messaging.interfaces.EncryptedMessage;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

public class Encryptor extends SerializationBase {

    /*
    public
    Encryptor(
        final java.security.PublicKey key
    ) {
        super(
            "RSA/ECB/NoPadding",
            javax.crypto.Cipher.ENCRYPT_MODE,
            key
        );
    }
    */
    
    EncryptedMessage
    encrypt(
        final PlaintextMessage plaintext,
        final java.util.List<java.security.PublicKey> recipients
    ) {
        return encrypt(
            PlaintextMessage.class.getPackage(), 
            new javax.xml.bind.JAXBElement<PlaintextMessage>(
                net.dushin.lethe.messaging.interfaces.Constants.PLAINTEXT_MESSAGE_QNAME,
                PlaintextMessage.class,
                plaintext
            ),
            recipients
        );
    }
    
    EncryptedMessage
    encrypt(
        final SignedMessage signed,
        final java.util.List<java.security.PublicKey> recipients
    ) {
        return encrypt(
            SignedMessage.class.getPackage(), 
            signed,
            recipients
        );
    }
    
    private EncryptedMessage
    encrypt(
        final Package pkg,
        final Object obj,
        final java.util.List<java.security.PublicKey> recipients
    ) {
        try {
            final EncryptedMessage ret = new EncryptedMessage();
            final byte[] serialized = serialize(pkg, obj);
            final SymmetricEncryptor encryptor = new SymmetricEncryptor();
            ret.setAlgorithm(encryptor.getSymmetricKey().getAlgorithm());
            final byte[] encrypted = encryptor.encrypt(serialized);
            final java.security.Key symmetricKey = encryptor.getSymmetricKey();
            ret.setRecipients(encryptKeyForRecipients(symmetricKey, recipients));
            ret.setEncryptedData(encrypted);
            return ret;
        } catch (final Exception e) {
            throw new RuntimeException("Error attempting to encrypt", e);
        }
    }
    
    private EncryptedKeyList
    encryptKeyForRecipients(
        final java.security.Key symmetricKey,
        final java.util.List<java.security.PublicKey> recipients
    ) {
        EncryptedKeyList ret = new EncryptedKeyList();
        for (java.security.PublicKey recipient : recipients) {
            ret.getItem().add(encryptKey(symmetricKey, recipient));
        }
        return ret;
    }
    
    private EncryptedKey
    encryptKey(
        final java.security.Key key,
        final java.security.PublicKey recipient
    ) {
        try {
            final javax.crypto.Cipher cipher = javax.crypto.Cipher.getInstance("RSA/ECB/NoPadding");
            cipher.init(
                javax.crypto.Cipher.ENCRYPT_MODE,
                recipient
            );
            final byte[] unencryptedKey = key.getEncoded();
            System.out.println("Unencrypted symmetric key:");
            System.out.println(HexDump.dump(unencryptedKey));
            final byte[] encryptedKey = cipher.doFinal(unencryptedKey);
            System.out.println("Encrypted symmetric key:");
            System.out.println(HexDump.dump(encryptedKey));
            final EncryptedKey ret = new EncryptedKey();
            ret.setData(encryptedKey);
            return ret;
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }
}
