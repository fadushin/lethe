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
__uses("SecureRandom.js");

/**
 * An EncryptorFactory is used to create an encryptor object,
 * which in turn is used to encrypt messages.
 *
 * An encryptor is created using the EncryptorFactory#createEncryptor
 * method, which is passed a paramter specification.
 *
 * Typical usage:
 *
 * var encryptor = EncryptorFactory.create({publicKey: ...});
 * var encryptedMessage = encryptor.encrypt(message)
 */
net_dushin_crypto.EncryptorFactory = {
        
    /**
     * The JSON-RPC object (for marshalling)
     */
    jsonrpc: imprt("jsonrpc"),
    
    /**
     * The Cipher type
     */
    Cipher: __import(this, "titaniumcore.crypto.Cipher"),
    
    /**
     * The PRNG type
     */
    SecureRandom: __import(this, "titaniumcore.crypto.SecureRandom"),
    
    /**
     * object createEncryptor(spec)
     *
     * @param       spec
     *              The constructor specification.  This parameter may contain
     *              the following elements:
     *
     *                  * publicKey:        the public key used to encrypt messages.
     *                                      This parameter is required
     *
     * @return      an encryptor object, implements the encrypt method
     *
     * @exception   IllegalArgument, if the public key is not provided
     */
    createEncryptor: function(spec) {
        //
        // Check the constructor parameters for a public key, to be used to
        // perform encryption.
        //
        if (!spec.publicKey) {
            throw net_dushin_foundation.ExceptionFactory.createIllegalArgumentException( 
                {message: "Missing publicKey parameter"}
            );
        }
        
        /**
         * the root scope
         */
        var root = this;

        /**
         * the private key
         */
        var rsa = net_dushin_crypto.KeyUtil.parseEncodedPublicKey(spec.publicKey);
        
        /**
         * Default cipher AES_CBC_PKCS7
         */
        var algorithm = spec.algorithm ? spec.algorithm : this.Cipher.RIJNDAEL;
        var mode = spec.mode ? spec.mode : "CBC";
        var padding = spec.padding ? spec.padding : "PKCS7";
        
        /**
         * the symmetric cipher
         */
        var cipher = this.Cipher.create(algorithm, "ENCRYPT", mode, padding);
        
        /**
         * The PRNG
         */
        var prng = new this.SecureRandom();
        
        /**
         * Generate a random symmetric key of size len
         */
        var generateSymmetricKey = function(len) {
            var buf = new Array(len);
            prng.nextBytes(buf);
            return buf;
        };

        //
        // Create and return the encryptor
        //
        return {
            /**
             * object encrypt(object)
             *
             * @param       object
             *              The object to encrypt.
             *
             * @return      the encrypted object, conforming to the definition at XXX
             */
            encrypt: function(object, recipient) {
                var symmetricKey = generateSymmetricKey(16);
                var maxLen = rsa.publicEncryptMaxSize();
                if (symmetricKey.length > maxLen) {
                    throw net_dushin_foundation.ExceptionFactory.createException( 
                        {message: "RSA key size is not big enough to encrypt symmetric key."}
                    );
                }
                var serializedObject = str2utf8(root.jsonrpc.marshall(object));
                var cipertext = cipher.execute(symmetricKey, serializedObject);
                var encryptedKey = rsa.publicEncrypt(symmetricKey);
                return {
                    type: "encrypted",
                    ciphertext: base64_encode(cipertext),
                    encryptedKey: base64_encode(encryptedKey),
                    algorithm: algorithm,
                    mode: mode,
                    padding: padding
                };
            }
        };
    }
};
