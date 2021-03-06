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
 * This class forms the base type for classes that operate
 * on signed messages (signing them and verifying signatures
 * over them).  It contains references to the JCA cryptographic
 * objects associated with these operations.
 */
abstract class SignatoryBase extends SerializationBase {

    /**
     * default signature algorithm
     */
    private static final String DEFAULT_SIGNATURE_ALGORITHM =
        "MD5withRSA";
    
    /**
     * default digest algorithms
     */
    private static final String DEFAULT_DIGEST_ALGORITHM =
        "SHA1";

    /**
     * JCA signature object used for signature creation and validation
     */
    protected final java.security.Signature signature;

    /**
     * JCA digest object used for hashing data.  Note that it's the
     * hashed data that's signed, not the data itself.
     */
    protected final java.security.MessageDigest digest;
    
    /**
     * default ctor instantiates member defaults
     */
    protected
    SignatoryBase() {
        try {
            this.signature = java.security.Signature.getInstance(DEFAULT_SIGNATURE_ALGORITHM);
            this.digest = java.security.MessageDigest.getInstance(DEFAULT_DIGEST_ALGORITHM);
        } catch (final java.security.NoSuchAlgorithmException e) {
            throw new RuntimeException("Error creating Signature", e);
        }
    }
    
    /**
     * Hash the input data
     */
    protected final byte[]
    hash(
        final byte[] data
    ) {
        try {
            return this.digest.digest(data);
        } catch (final Exception e) {
            throw new RuntimeException("Error hashing", e);
        }
    }
    
}
