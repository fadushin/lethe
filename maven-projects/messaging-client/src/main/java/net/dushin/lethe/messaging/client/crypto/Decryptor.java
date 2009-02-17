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

/**
 * This class is used to decrypt encrypted messages.  It must be initialized
 * with a private key, which must correspond with the public key used to
 * encrypt the message.
 */
public class Decryptor extends CryptorBase {

    /**
     * @param       key
     *              The private key used to decrypt messages
     */
    public
    Decryptor(
        final java.security.PrivateKey key
    ) {
        super(
            "RSA/ECB/NoPadding",
            javax.crypto.Cipher.DECRYPT_MODE,
            key
        );
    }
    
    /**
     * Decrypt an EncryptedMessage.
     *
     * @param       encrypted
     *              The EncryptedMessage to decrypt.
     *
     * @return      the decrypted message.  Typically, this object will
     *              be an instance of a PlaintextMessage or a SignedMessage,
     *              depending on whether the sender has signed the encrypted
     *              message.
     */
    public Object
    decrypt(final EncryptedMessage encrypted) {
        try {
            final javax.crypto.SecretKey decryptedKey = 
                findDecryptedKey(
                    encrypted.getRecipients(), encrypted.getAlgorithm()
                );
            final byte[] decryptedData = decryptData(decryptedKey, encrypted.getEncryptedData());
            return deserialize(
                EncryptedMessage.class.getPackage(),
                decryptedData
            );
        } catch (final Exception e) {
            throw new RuntimeException("Error decrypting", e);
        }
    }
    
    //
    // internal operations
    //
    
    /**
     * @return      the decrypted secret key, from a list of encrypted keys.
     *              This operation will return a non-null value if there is
     *              an encrypted key in the specified list that was encrypted
     *              using the public key corresponding to the private key
     *              with which this Decryptor was initialized.  Otherwise,
     *              the operation will raise an exception.
     */
    private javax.crypto.SecretKey
    findDecryptedKey(
        final EncryptedKeyList encryptedKeys,
        final String algorithm
    ) {
        for (EncryptedKey encryptedKey : encryptedKeys.getItem()) {
            try {
                return decryptKey(encryptedKey, algorithm);
            } catch (final Exception e) {
                continue;
            }
        }
        throw new RuntimeException("Key not found");
    }
    
    /**
     * @return      the decrypted secret key, from the specified EncryptedKey.
     *              This operation will raise an exception if decryption fails.
     */
    private javax.crypto.SecretKey
    decryptKey(
        final EncryptedKey encryptedKey,
        final String algorithm
    ) {
        try {
            final byte[] encryptedKeyData = encryptedKey.getData();
            final byte[] decryptedKey = this.cipher.doFinal(encryptedKeyData);
            Logger.logBuffer(
                "Encrypted key prior to decryption:",
                encryptedKeyData
            );
            Logger.logBuffer(
                "Decrypted key:",
                decryptedKey
            );
            //
            // TODO something is awry here -- I need to ignore the first
            // 32 bytes of the decrypted data -- they are all 0 for some
            // reason I need to look into
            //
            final javax.crypto.spec.SecretKeySpec spec = 
                new javax.crypto.spec.SecretKeySpec(
                    decryptedKey, 
                    32, 32, 
                    algorithm
                );
            return spec;
        } catch (final Exception e) {
            throw new RuntimeException("Error decrypting", e);
        }
    }
    
    /**
     * @return      the result of decrypting the specified data with the
     *              specified secret key.
     */
    private static byte[]
    decryptData(
        final javax.crypto.SecretKey key,
        final byte[] data
    ) {
        try {
            final SymmetricDecryptor decryptor = new SymmetricDecryptor(key);
            return decryptor.decrypt(data);
        } catch (final Exception e) {
            throw new RuntimeException("Error decrypting", e);
        }
    }
}
