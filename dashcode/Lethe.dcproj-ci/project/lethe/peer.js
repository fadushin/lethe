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

Lethe.Peer = Class.create(
    Lethe.KVO,
    {
        constructor: function(peerId, name, pubKey, verifier, encryptTo) {
            this.base(
                {
                    peerId: peerId,
                    name: name, 
                    pubKey: pubKey, 
                    encryptTo: encryptTo,
                    //
                    //
                    //
                    encryptor: net_dushin_crypto.EncryptorFactory.createEncryptor(
                        {publicKey: pubKey}
                    ),
                    verifier: verifier ? verifier : net_dushin_crypto.VerifierFactory.createVerifier(
                        {publicKey: pubKey}
                    )
                }
            );
        }
    }
);

Lethe.Peer.parse = function(obj) {
    var deblob = net_dushin_foundation.Serialization.deserialize(obj.blob);
    var verifier = net_dushin_crypto.VerifierFactory.createVerifier(
        {publicKey: deblob.pubKey}
    );
    try {
        var result = verifier.verify(deblob.signedData);
        if (!result.status) {
            alert("Signature verification failed!");
            throw "Signature verification failed";
        }
        var verifiedData = result.value;
        return new Lethe.Peer(
            obj.id,
            verifiedData.name,
            deblob.pubKey,
            verifier,
            false
        );
    } catch (e) {
        alert(e);
        throw e;
    }
};
