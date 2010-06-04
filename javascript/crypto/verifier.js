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
            throw net_dushin_exception.ExceptionFactory.createIllegalArgumentException( 
                {message: "Missing publicKey parameter" }
            );
        }
        
        /**
         * The JSON-RPC object (for marshalling)
         */
        var jsonrpc = imprt("jsonrpc");

        /**
         * the public key
         */
        var publicKey = spec.publicKey;

        //
        // Create and return the verifier
        //
        return {
            /**
             * boolean verify(object)
             *
             * @param       object
             *              The signed object to verify.  This must be a signed object,
             *              conforming to the definition at XXX
             *
             * @return      true, if the signature over the data is valid; false, otherwise
             *
             * @exception   if the supplied object is not a signed object
             */
            verify: function(object) {
                if (object.type !== "signed") {
                    throw net_dushin_exception.ExceptionFactory.createException(
                        {
                            message: "Message is not a signed object message."
                        }
                    );
                }
                // assert object.serialized
                var serializedObject = object.serialized;
                var hashValue = sha1Hash(serializedObject);
                // assert object.hash
                if (hashValue !== object.hash) {
                    return false;
                }
                // TODO signature validation
                return false;
            }
        };
    }
};

