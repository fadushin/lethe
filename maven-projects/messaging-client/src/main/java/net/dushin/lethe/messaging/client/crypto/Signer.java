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

import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

/**
 * This class is used to sign plaintext messages.  It must be initialized
 * with the private key material used to do the signing, and upon successful
 * signing, will return a SignedMessage structure containing the serialized
 * data that was signed, together with the cryptographic signature.
 *
 * Note that the signature is actually over a SHA1 hash of the serialized
 * contents of the plaintext message itself.  The hash itself is not
 * delivered in the SignedMessage, but instead must be computed from
 * the serialized data.
 *
 * The complement of this class is the Verifier class, which verifies
 * the signature over a plaintext message, and returns the deserialized 
 * PlaintextMessage as a result.
 */
public class Signer extends SignatoryBase {

    /**
     * Create a Signer using the designated private key.  This key will
     * be used for all sign operations used by this Signer instance.
     */
    public 
    Signer(
        final java.security.PrivateKey key
    ) {
        super();
        try {
            this.signature.initSign(key);
        } catch (final java.security.InvalidKeyException e) {
            throw new RuntimeException("Error initializing key", e);
        }
    }

    /**
     * Sign the designated plaintext message.
     *
     * @param       plaintext
     *              The PlaintextMessage to Sign
     *
     * @return      a SignedMessage containing the serialized form
     *              of the supplied plaintext message, together with
     *              a signature over (a hash of) the serialized form
     *              of the plaintext message.
     */
    public SignedMessage
    sign(
        final PlaintextMessage plaintext
    ) {
        return sign(
            PlaintextMessage.class.getPackage(), 
            new javax.xml.bind.JAXBElement<PlaintextMessage>(
                net.dushin.lethe.messaging.interfaces.Constants.PLAINTEXT_MESSAGE_QNAME,
                PlaintextMessage.class,
                plaintext
            )
        );
    }

    //
    // internal operations
    //
    
    /**
     * Sign the specified jaxb object, assumed to reside in the specified package.
     */
    private SignedMessage
    sign(
        final Package pkg,
        final Object obj
    ) {
        try {
            final SignedMessage ret = new SignedMessage();
            //
            // Get the serialized form of the input plaintext
            // message, and compute the SHA1 hash over the serialized
            // form.  Then, sign the hash value.
            //
            final byte[] serialized = serialize(pkg, obj);
            Logger.logBuffer(
                "Serialized message:",
                serialized
            );
            final byte[] hash = hash(serialized);
            Logger.logBuffer(
                "Hash of serialized data:",
                hash
            );
            final byte[] signature = sign(hash);
            Logger.logBuffer(
                "Signature over hash:",
                signature
            );
            //
            // set the appropriate fieldson the return struct
            //
            ret.setSerializedMessage(serialized);
            ret.setSignature(signature);
            return ret;
        } catch (final Exception e) {
            throw new RuntimeException("Error signing message", e);
        }
    }
    
    /**
     * Sign the data
     */
    private byte[]
    sign(final byte[] data) {
        try {
            this.signature.update(data);
            return this.signature.sign();
        } catch (final java.security.SignatureException e) {
            throw new RuntimeException("Error signing data", e);
        }
    }
}
