//
// Copyright (c) dushin.net
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of dushin.net nor the
//       names of its contributors may be used to endorse or promote products
//       derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

// requires function imprt
// requires function net_dushin_exception.ExceptionFactory.createException
// requires function net_dushin_exception.ExceptionFactory.createIllegalArgumentException
__uses("binary.js");
__uses("SHA.js");

/**
 * A VerifierFactory is used to create a verifier object,
 * which in turn is used to verify signed messages.
 *
 * A Verifier is created using the VerifierFactory#createVerifier
 * method, which is passed a paramter specification.
 *
 * Typical usage:
 *
 * var verifier = Verifier.create({publicKey: ...});
 * try {
 *     var ok = verifier.verify(message);
 * } catch (exception) {
 *     ...
 * }
 */
net_dushin_crypto.VerifierFactory = {

    /**
     * The SHA type
     */
    SHA: __import(this, "titaniumcore.crypto.SHA"),

    /**
     * object createVerifier(spec)
     *
     * @param       spec
     *              The constructor specification.  This parameter may contain
     *              the following elements:
     *
     *                  * publicKey:        the public key used to verify messages.
     *                                      This parameter is required
     *
     * @return      a verifier object, implements the verify method
     *
     * @exception   IllegalArgument, if the public key is not provided
     */
    createVerifier: function(spec) {
        //
        // Check the constructor parameters for a public key, to be used to
        // perform signature verification.
        //
        if (!spec.publicKey) {
            throw net_dushin_foundation.ExceptionFactory.createIllegalArgumentException( 
                {message: "Missing publicKey parameter" }
            );
        }
        
        /**
         * The JSON-RPC object (for marshalling)
         */
        var jsonrpc = imprt("jsonrpc");

        /**
         * the private key
         */
        var rsa = net_dushin_crypto.KeyUtil.parseEncodedPublicKey(spec.publicKey);
        
        /**
         * the hash algorithm (SHA-1)
         */
        var sha = this.SHA.create("SHA-1");

        //
        // Create and return the verifier
        //
        return {
            /**
             * {status: <boolean>, value: <object>} verify(object)
             *
             * @param       object
             *              The signed object to verify.  This must be a signed object,
             *              conforming to the definition at XXX
             *
             * @return      an object, with the following elements:
             *                  status:     true, if the signature over the data is valid; 
             *                              false, otherwise
             *                  value:      the deserialized form of the input object, if
             *                              status is true; undefined, otherwise.
             *
             * @exception   if the supplied object is not a signed object
             */
            verify: function(object) {
                if (object.type !== "signed") {
                    throw net_dushin_foundation.ExceptionFactory.createException(
                        {
                            message: "Message is not a signed object message."
                        }
                    );
                }
                // assert object.serialized
                //
                // check that the hash for the serialized object matches the
                // hash in the signed message
                //
                var serializedObject = base64_decode(object.serialized);
                var hashValue = sha.hash(serializedObject);
                // assert object.hash
                if (base64_encode(hashValue) !== object.hash) {
                    return {status: false};
                }
                //
                // Check that the decrypted signature is the hash (but chop
                // off the bytes after the hash length, due to padding)
                //
                var expected = rsa.publicDecrypt(base64_decode(object.signature));
                var decrypted = base64_encode(expected.slice(0, hashValue.length));
                if (decrypted !== object.hash) {
                    return {status: false};
                }
                //
                // return the unmarshalled object, if the signature checks out
                //
                var deserializedObject = jsonrpc.unmarshall(utf82str(serializedObject));
                return {status: true, value: deserializedObject};
            }
        };
    }
};

