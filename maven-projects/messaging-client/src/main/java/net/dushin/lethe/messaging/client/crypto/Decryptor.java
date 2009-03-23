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

import net.dushin.lethe.messaging.common.log.LogUtil;
import net.dushin.lethe.messaging.interfaces.EncryptedKey;
import net.dushin.lethe.messaging.interfaces.EncryptedMessage;

/**
 * This class is used to decrypt encrypted messages.  It must be initialized
 * with a private key, which must correspond with the public key used to
 * encrypt the message.
 */
public class Decryptor extends CryptorBase {
    
    /**
     * the Logger instance to be used by this class
     */
    private static final java.util.logging.Logger LOGGER =
        java.util.logging.Logger.getLogger(Decryptor.class.getName());
    
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
        //
        //
        //
        for (EncryptedKey encryptedKey : encrypted.getRecipients().getItem()) {
            try {
                final javax.crypto.SecretKey decryptedKey =
                    decryptKey(encryptedKey, encrypted.getAlgorithm());
                final byte[] decryptedData = decryptData(decryptedKey, encrypted.getEncryptedData());
                return deserialize(
                    EncryptedMessage.class.getPackage(),
                    decryptedData
                );
            } catch (final Exception e) {
                LogUtil.logException(
                    LOGGER, 
                    java.util.logging.Level.FINE, 
                    e, 
                    "An error occurred decrypting an encrypted message."
                );
                continue;
            }
        }
        throw new RuntimeException("Key not found");
    }
    
    //
    // internal operations
    //
    
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
            LogUtil.logBuffer(
                "Encrypted key prior to decryption:",
                encryptedKeyData
            );
            LogUtil.logBuffer(
                "Decrypted key:",
                decryptedKey
            );
            //
            // TODO something is awry here -- I need to ignore the first
            // 32 bytes of the decrypted data -- they are all 0 for some
            // reason I need to look into
            //
            final int idx = decryptedKey.length - 32;
            final javax.crypto.spec.SecretKeySpec spec = 
                new javax.crypto.spec.SecretKeySpec(
                    decryptedKey, 
                    idx, 32, 
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
