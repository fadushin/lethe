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

public class Decryptor extends CryptorBase {

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
    
    Object
    decrypt(final EncryptedMessage encrypted) {
        try {
            final javax.crypto.SecretKey decryptedKey = decryptKey(encrypted.getEncryptedKey());
            final byte[] decryptedData = decryptData(decryptedKey, encrypted.getEncryptedData());
            System.out.println(new String(decryptedData));
            return deserialize(
                EncryptedMessage.class.getPackage(),
                decryptedData
            );
        } catch (final Exception e) {
            throw new RuntimeException("Error decrypting", e);
        }
    }
    
    private javax.crypto.SecretKey
    decryptKey(
        final byte[] encryptedKey
    ) {
        try {
            final byte[] decryptedKey = this.cipher.doFinal(encryptedKey);
            final javax.crypto.spec.SecretKeySpec spec = 
                new javax.crypto.spec.SecretKeySpec(decryptedKey, 32, 32, "AES");
            return spec;
            /*
            final javax.crypto.SecretKeyFactory factory =
                javax.crypto.SecretKeyFactory.getInstance("AES");
            return factory.generateSecret(spec);
            */
        } catch (final Exception e) {
            throw new RuntimeException("Error decrypting", e);
        }
    }
    
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
