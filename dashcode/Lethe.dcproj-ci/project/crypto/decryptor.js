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
__uses("Cipher.js");

/**
 * An DecryptorFactory is used to create an decryptor object,
 * which in turn is used to edecrypt messages.
 *
 * A decryptor is created using the DecryptorFactory#createDecryptor
 * method, which is passed a paramter specification.
 *
 * Typical usage:
 *
 * var decryptor = DecryptorFactory.create({privateKey: ...});
 * var message = encryptor.decrypt(encryptedMessage)
 */
net_dushin_crypto.DecryptorFactory = {
    
    /**
     * The Cipher type
     */
    Cipher: __import(this, "titaniumcore.crypto.Cipher"),
        
    /**
     * The JSON-RPC object (for marshalling)
     */
    jsonrpc: JSOlait.imprt("jsonrpc"),
    
    /**
     * object createDecryptor(spec)
     *
     * @param       spec
     *              The constructor specification.  This parameter may contain
     *              the following elements:
     *
     *                  * privateKey:       the private key used to decrypt messages.
     *                                      This parameter is required
     *
     * @return      an decryptor object, implements the decrypt method
     *
     * @exception   IllegalArgument, if the private key is not provided
     */
    createDecryptor: function(spec) {
        //
        // Check the constructor parameters for a private key, to be used to
        // perform decryption.
        //
        if (!spec.privateKey) {
            throw net_dushin_foundation.ExceptionFactory.createIllegalArgumentException( 
                {message: "Missing privateKey parameter"}
            );
        }
        
        /**
         * the root scope
         */
        var root = this;
        
        /**
         * the private key
         */
        var rsa = net_dushin_crypto.KeyUtil.parseEncodedPrivateKey(spec.privateKey);
        
        //
        // Create and return the encryptor
        //
        return {
            /**
             * object decrypt(object)
             *
             * @param       object
             *              The object to decrypt.
             *
             * @return      the encrypted object, conforming to the definition at XXX
             */
            decrypt: function(object) {
                if (object.type !== "encrypted") {
                    throw net_dushin_foundation.ExceptionFactory.createException(
                        {
                            message: "Message is not a signed object message."
                        }
                    );
                }
                var cipher = root.Cipher.create(object.algorithm, "DECRYPT", object.mode, object.padding);
                var symmetricKey;
                if (object.encryptedKey) {
                    symmetricKey = rsa.privateDecrypt(base64_decode(object.encryptedKey));
                } else if (object.encryptedKeys) {
                    symmetricKey = net_dushin_foundation.Lists.mapFirst(
                        function(encryptedKey) {
                            try {
                                return rsa.privateDecrypt(base64_decode(encryptedKey));
                            } catch (e) {
                                return false;
                            }
                        },
                        object.encryptedKeys
                    );
                    if (!symmetricKey) {
                        throw "Unable to decrypt any of the encrypted symmetric keys";
                    }
                } else {
                    throw "Encrypted message must contain at most one of encryptedKey or encryptedKeys";
                }
                var plaintext = utf82str(cipher.execute(symmetricKey.slice(0, 16), base64_decode(object.ciphertext)));
                return root.jsonrpc.unmarshall(plaintext);
            }
        };
    }
};
