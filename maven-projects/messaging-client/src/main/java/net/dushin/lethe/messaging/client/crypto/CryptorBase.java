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

/**
 * This class forms a base class for cryptographic types that require
 * access to a single Cipher instance, which is initialized in this
 * class's constructor.
 */
abstract class CryptorBase extends SerializationBase {

    /**
     * The Cipher instance used by this Cryptor
     */
    protected final javax.crypto.Cipher cipher;

    /**
     * @param       spec
     *              The cipher algorithm/block/pad spec
     *
     * @param       mode
     *              The cryptographic mode (encrypt or decrypt)
     *
     * @param       key
     *              The cryptographic key used to initialize the Cipher
     */
    protected
    CryptorBase(
        final String spec,
        final int mode,
        final java.security.Key key
    ) {
        try {
            this.cipher = javax.crypto.Cipher.getInstance(spec);
            cipher.init(
                mode,
                key
            );
        } catch (final Exception e) {
            throw new RuntimeException("Error initializing Cipher", e);
        }
    }
}
