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
import net.dushin.lethe.messaging.interfaces.SignedMessage;

/**
 * This object is used to verify signatures over signed messages.
 * It must be initialized with the public key used to validate
 * the signatures.
 */
public class Verifier extends SignatoryBase {

    /**
     * @param       key
     *              The public key with which signatures will be validated.
     */
    public 
    Verifier(
        final java.security.PublicKey key
    ) {
        super();
        try {
            this.signature.initVerify(key);
        } catch (final java.security.InvalidKeyException e) {
            throw new RuntimeException("Error initializing signature", e);
        }
    }

    /**
     * Verify the signature on a signed message
     *
     * @param       signed
     *              The SignedMessage to verify the signature over
     *
     * @return      the deserialized form of the serialized object stored in the
     *              SignedMessage, if verification succeds.
     *
     * @throws      a runtime exception, if verification or deserialization fails
     */
    public final Object
    verify(
        final SignedMessage signed
    ) {
        try {
            final byte[] serialized = signed.getSerializedMessage();
            LogUtil.logBuffer(
                "Serialized message:",
                serialized
            );
            final byte[] hash = hash(serialized);
            LogUtil.logBuffer(
                "Hash of serialized data:",
                hash
            );
            final byte[] sigData = signed.getSignature();
            LogUtil.logBuffer(
                "Signature over hash:",
                sigData
            );
            final boolean valid = verify(hash, sigData);
            if (!valid) {
                throw new RuntimeException("Signature validation failed");
            }
            return deserialize(
                SignedMessage.class.getPackage(),
                serialized
            );
        } catch (final Exception e) {
            throw new RuntimeException("Error verifying message", e);
        }
    }
    
    //
    // internal operations
    //
    
    /**
     * verify the signature over the (hashed) data
     */
    private boolean
    verify(
        final byte[] data,
        final byte[] sigData
    ) {
        try {
            this.signature.update(data);
            return this.signature.verify(sigData);
        } catch (final java.security.SignatureException e) {
            throw new RuntimeException("Error verifyig signature data", e);
        }
    }
}
