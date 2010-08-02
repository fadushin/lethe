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

Lethe.Identity = Class.create(
    Lethe.KVO,
    {
        constructor: function(name, privKey, pubKey) {
            this.base(
                {
                    name: name, 
                    privKey: privKey, 
                    pubKey: pubKey,
                }
            );
            //
            // derived properties
            //
            this.signer = privKey ? net_dushin_crypto.SignerFactory.createSigner(
                {privateKey: privKey}
            ) : null;
            this.decryptor = privKey ? net_dushin_crypto.DecryptorFactory.createDecryptor(
                {privateKey: privKey}
            ) : null;
            this.peer = pubKey ? this.toPeerObject() : null;
        },
        
        assign: function(identity) {
            this.privKey = identity.privKey;
            this.setValueForKeyPath(identity.pubKey, "pubKey");
            this.signer = identity.privKey ? net_dushin_crypto.SignerFactory.createSigner(
                {privateKey: identity.privKey}
            ) : null;
            this.decryptor = identity.privKey ? net_dushin_crypto.DecryptorFactory.createDecryptor(
                {privateKey: identity.privKey}
            ) : null;
            this.setValueForKeyPath(identity.name, "name");
            this.peer = identity.pubKey ? this.toPeerObject() : null;
        },
        
        getName: function() {
            return this.name;
        },
        
        setName: function(nom) {
            this.name = nom;
        },
        
        regenerate: function() {
            var rsa = net_dushin_crypto.KeyUtil.createRSA();
            rsa.generate(512, 65537);
            var lprivKey = net_dushin_crypto.KeyUtil.encodePrivateKey(rsa);
            var lpubKey = net_dushin_crypto.KeyUtil.encodePublicKey(rsa);
            this.setValueForKeyPath(
                lprivKey, 
                "privKey"
            );
            this.setValueForKeyPath(
                lpubKey, 
                "pubKey"
            );
            var lsigner = net_dushin_crypto.SignerFactory.createSigner({privateKey: lprivKey});
            this.setValueForKeyPath(
                lsigner, 
                "signer"
            );
        },
        
        serialize: function() {
            return this.base(
                {
                    name: this.name,
                    privKey: this.privKey,
                    pubKey: this.pubKey
                }
            );
        },

        createPeerBlob: function() {
            var name = this.valueForKeyPath("name");
            var pubKey = this.valueForKeyPath("pubKey");
            var signer = this.signer; // this.valueForKeyPath("signer");
            var signedObject = signer.sign({name: name});
            return net_dushin_foundation.Serialization.serialize(
                {
                    pubKey: pubKey, 
                    signedData: signedObject
                }
            );
        },
        
        toPeerObject: function() {
            var blob = this.createPeerBlob();
            var id = net_dushin_foundation.Serialization.sha1Hash(blob);
            return {
                id: id,
                blob: blob
            };
        },
        
        setKey: function(privKey, pubKey) {
            this.privKey = privKey;
            this.pubKey = pubKey;
            this.signer = net_dushin_crypto.SignerFactory.createSigner(
                {privateKey: privKey}
            );
        }
    }
);

Lethe.Identity.deserialize = function(str) {
    var obj = Lethe.KVO.deserialize(str);
    return new Lethe.Identity(
        obj.name, obj.privKey, obj.pubKey
    );
};
