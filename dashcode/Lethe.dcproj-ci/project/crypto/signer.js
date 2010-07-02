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
 * A SignerFactory is used to create a signer object,
 * which in turn is used to sign messages.
 *
 * A signer is created using the SignerFactory#createSigner
 * method, which is passed a paramter specification.
 *
 * Typical usage:
 *
 * var signer = SignerFactory.create({privateKey: ...});
 * var signedMessage = signer.sign(message)
 */
net_dushin_crypto.SignerFactory = {
    
    /**
     * The BigInteger type
     */
    // BigInteger: __import(this, "titaniumcore.crypto.BigInteger"),
    
    /**
     * The SHA type
     */
    SHA: __import(this, "titaniumcore.crypto.SHA"),
    
    /**
     * object createSigner(spec)
     *
     * @param       spec
     *              The constructor specification.  This parameter may contain
     *              the following elements:
     *
     *                  * privateKey:       the private key used to sign messages.
     *                                      This parameter is required
     *
     * @return      a signer object, implements the sign method
     *
     * @exception   IllegalArgument, if the private key is not provided
     */
    createSigner: function(spec) {
        //
        // Check the constructor parameters for a private key, to be used to
        // perform signature verification.
        //
        if (!spec.privateKey) {
            throw net_dushin_foundation.ExceptionFactory.createIllegalArgumentException( 
                {message: "Missing privateKey parameter"}
            );
        }
        

        /**
         * The JSON-RPC object (for marshalling)
         */
        var jsonrpc = JSOlait.imprt("jsonrpc");

        /**
         * the private key
         */
        var rsa = net_dushin_crypto.KeyUtil.parseEncodedPrivateKey(spec.privateKey);
        
        /**
         * the hash algorithm (SHA-1)
         */
        var sha = this.SHA.create("SHA-1");

        //
        // Create and return the signer
        //
        return {
            /**
             * boolean sign(object)
             *
             * @param       object
             *              The object to sign.
             *
             * @return      the signed object, conforming to the definition at XXX
             *
             * @exception   if the supplied object is not a signed object
             */
            sign: function(object) {
                var serializedObject = str2utf8(jsonrpc.marshall(object));
                var hashValue = sha.hash(serializedObject);
                var maxLen = rsa.privateEncryptMaxSize();
                if (hashValue.length > maxLen) {
                    throw net_dushin_foundation.ExceptionFactory.createException( 
                        {message: "Key size is not big enough to sign hash."}
                    );
                }
                var signatureValue = rsa.privateEncrypt(hashValue);
                return {
                    type: "signed",
                    serialized: base64_encode(serializedObject),
                    hash: base64_encode(hashValue),
                    signature: base64_encode(signatureValue)
                };
            }
        };
    }
};
