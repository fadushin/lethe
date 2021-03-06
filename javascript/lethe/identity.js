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


__uses("BigInteger.init1.js");
__uses("BigInteger.init2.js");
__uses("RSA.init1.js");
__uses("RSA.init2.js");
__uses("RSA.init3.js");


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
            this.peer = pubKey ? new Lethe.Peer({name: name, pubKey: pubKey, isTrusted: true}) : null;
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
            this.peer = identity.pubKey ? new Lethe.Peer({name: identity.name, pubKey: identity.pubKey, isTrusted: true}) : null;
        },
        
        getName: function() {
            return this.name;
        },
        
        setName: function(nom) {
            this.name = nom;
        },
        
        getPeer: function() {
            return this.peer;
        },
        
        regenerate: function(progress) {
            var rsa = net_dushin_crypto.KeyUtil.createRSA();
            var that = this;
            
            var result = function(data, rsaResult) {
                console.log("result(" + data + ", " + rsa + ")");
                var lprivKey = net_dushin_crypto.KeyUtil.encodePrivateKey(rsa);
                var lpubKey = net_dushin_crypto.KeyUtil.encodePublicKey(rsa);
                that.setValueForKeyPath(
                    lprivKey, 
                    "privKey"
                );
                that.setValueForKeyPath(
                    lpubKey, 
                    "pubKey"
                );
                var lsigner = net_dushin_crypto.SignerFactory.createSigner({privateKey: lprivKey});
                that.setValueForKeyPath(
                    lsigner, 
                    "signer"
                );
            };
            var done = function(succeeded, count, time ,startTime, finishTime) {
                console.log("RSA keygen done(" + succeeded + ", " + count + ", " + time + ", " + startTime + ", " + finishTime + ")");
            };
            rsa.generateAsync(
                512, 65537, 
                progress, 
                result, 
                done
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
        
        toPeerObject: function() {
            return this.peer.toPeerObject();
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
