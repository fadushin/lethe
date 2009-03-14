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

import net.dushin.lethe.messaging.client.log.LogUtil;

/**
 * This class is used to encrypt data using a secret key.
 *
 * This class supports a default constructor, which will initialize
 * an instance using a randmly generated secret key.
 */
class SymmetricEncryptor extends CryptorBase {

    /**
     * the default symmetric key algorithm to use
     */
    private static final String DEFAULT_ALGORITHM = "AES"; // "DESede/CBC/PKCS5Padding";
    
    /**
     * The secret key with which this class was initialized
     */
    private final java.security.Key key;

    /**
     * Create an instance using a randomly generated secret key.
     */
    SymmetricEncryptor() {
        this(generateSymmetricKey(DEFAULT_ALGORITHM));
    }

    /**
     * Create an instance using the specified secret key.
     */
    SymmetricEncryptor(
        final javax.crypto.SecretKey key
    ) {
        super(
            DEFAULT_ALGORITHM,
            javax.crypto.Cipher.ENCRYPT_MODE,
            key
        );
        this.key = key;
    }
    
    /**
     * @return      a reference to the secret key with which this
     *              class instance was initialized.
     */
    java.security.Key
    getSymmetricKey() {
        return this.key;
    }
    
    /**
     * @param       data
     *              the data to encrypt
     *
     * @return      the result of encrypting the specified data
     *              using the secret key with which this class instance
     *              was initialized.
     */
    byte[]
    encrypt(
        final byte[] data
    ) {
        try {
            final byte[] ret = this.cipher.doFinal(data);
            LogUtil.logBuffer(
                "Data prior to encryption:",
                data
            );
            LogUtil.logBuffer(
                "Encrypted data:",
                ret
            );
            return ret;
        } catch (final Exception e) {
            throw new RuntimeException("Error encrypting data", e);
        }
    }
    
    /**
     * @return      a randomly generated secret key
     */
    private static javax.crypto.SecretKey
    generateSymmetricKey(
        final String algorithm
    ) {
        try {
            final javax.crypto.KeyGenerator generator =
                javax.crypto.KeyGenerator.getInstance(algorithm);
            generator.init(256);
            return generator.generateKey();
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }
}
