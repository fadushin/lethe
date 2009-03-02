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

import net.dushin.lethe.messaging.interfaces.EncryptedKey;
import net.dushin.lethe.messaging.interfaces.EncryptedKeyList;
import net.dushin.lethe.messaging.interfaces.EncryptedMessage;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

/**
 * An Encryptor is responsible for encrypting a message to a
 * collection of recipients, where each recipient is encapsulated
 * by a java.security.PublicKey.
 *
 * Unlike other cryptographic objects in this package, this object
 * effectively has no state associated with it, and can be constructed
 * with no initial data; however, note that the encryption operations on
 * this type do require a collection of public keys.
 */
public class Encryptor extends SerializationBase {
    
    /**
     * @param msg
     * @param recipients
     * @return
     */
    public EncryptedMessage
    encrypt(
        final Object msg,
        final java.util.Collection<java.security.PublicKey> recipients
    ) {
        if (msg instanceof PlaintextMessage) {
            return encrypt((PlaintextMessage) msg, recipients);
        } else if (msg instanceof SignedMessage) {
            return encrypt((SignedMessage) msg, recipients);
        } else {
            throw new RuntimeException("Unsupported type: " + msg.getClass().getName());
        }
    }

    /**
     * Encrypt a plaintext message for a collection of recipients.
     *
     * @param       plaintext
     *              The PlaintextMessage to encrypt
     *
     * @param       recipients
     *              The list of recipients to whom the message should
     *              be encrypted
     *
     * @return      the result of encrypting the supplied message
     *              for the collection of intended recipients, encapsulated
     *              by a collection of public keys.  The resulting 
     *              EncryptedMessage is as described in the corresponding
     *              message.idl, from which this type is derived.
     */
    public EncryptedMessage
    encrypt(
        final PlaintextMessage plaintext,
        final java.util.Collection<java.security.PublicKey> recipients
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
    
    /**
     * Encrypt a signed message for a collection of recipients.
     *
     * @param       signed
     *              The SignedMessage to encrypt
     *
     * @param       recipients
     *              The list of recipients to whom the message should
     *              be encrypted
     *
     * @return      the result of encrypting the supplied message
     *              for the collection of intended recipients, encapsulated
     *              by a collection of public keys.  The resulting 
     *              EncryptedMessage is as described in the corresponding
     *              message.idl, from which this type is derived.
     */
    public EncryptedMessage
    encrypt(
        final SignedMessage signed,
        final java.util.Collection<java.security.PublicKey> recipients
    ) {
        return encrypt(
            SignedMessage.class.getPackage(), 
            new javax.xml.bind.JAXBElement<SignedMessage>(
                net.dushin.lethe.messaging.interfaces.Constants.SIGNED_MESSAGE_QNAME,
                SignedMessage.class,
                signed
            ),
            recipients
        );
    }
    
    //
    // internal operations
    //
    
    /**
     * Encrypt the object defined in the supplied package for the intended
     * recipients.
     */
    private EncryptedMessage
    encrypt(
        final Package pkg,
        final Object obj,
        final java.util.Collection<java.security.PublicKey> recipients
    ) {
        try {
            final EncryptedMessage ret = new EncryptedMessage();
            //
            // Create a transient symmetric key to encrypt the message
            //
            final SymmetricEncryptor encryptor = new SymmetricEncryptor();
            ret.setAlgorithm(encryptor.getSymmetricKey().getAlgorithm());
            //
            // Serialize the message to be encrypted, and encrypt it using
            // the symmetric key
            //
            final byte[] serialized = serialize(pkg, obj);
            final byte[] encrypted = encryptor.encrypt(serialized);
            ret.setEncryptedData(encrypted);
            //
            // Encrypt the symmetric key for the recipients
            //
            ret.setRecipients(
                encryptKeyForRecipients(encryptor.getSymmetricKey(), recipients)
            );
            return ret;
        } catch (final Exception e) {
            throw new RuntimeException("Error attempting to encrypt", e);
        }
    }
    
    /**
     * Encrypt the specified symmetric key for the collection of
     * recipients.  The result will be a list of encrypt keys, each of
     * which is the encrypted form of the symmetric key used to encrypt
     * the actual message.
     */
    private EncryptedKeyList
    encryptKeyForRecipients(
        final java.security.Key symmetricKey,
        final java.util.Collection<java.security.PublicKey> recipients
    ) {
        EncryptedKeyList ret = new EncryptedKeyList();
        for (java.security.PublicKey recipient : recipients) {
            ret.getItem().add(encryptKey(symmetricKey, recipient));
        }
        return ret;
    }
    
    /**
     * Encrypt a (symmetric) key using the public key representing the
     * recipient.
     */
    private EncryptedKey
    encryptKey(
        final java.security.Key key,
        final java.security.PublicKey recipient
    ) {
        try {
            //
            // Create the Cipher from the public key
            //
            final javax.crypto.Cipher cipher = 
                javax.crypto.Cipher.getInstance("RSA/ECB/NoPadding");
            cipher.init(
                javax.crypto.Cipher.ENCRYPT_MODE,
                recipient
            );
            //
            // Encrypt the encoded form of the (symmetric) key
            //
            final byte[] unencryptedKey = key.getEncoded();
            Logger.logBuffer(
                "Encrypting symmetric key:",
                unencryptedKey
            );
            final byte[] encryptedKey = cipher.doFinal(unencryptedKey);
            Logger.logBuffer(
                "Encrypted symmetric key:",
                encryptedKey
            );
            //
            // Wrap the encrypted key in an EncryptedKey struct
            //
            final EncryptedKey ret = new EncryptedKey();
            ret.setData(encryptedKey);
            return ret;
        } catch (final Exception e) {
            throw new RuntimeException("Error encrypting symmetric key", e);
        }
    }
}
