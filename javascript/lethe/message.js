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

Lethe.Message = Class.create(
    Lethe.KVO,
    {
        constructor: function(spec) {
            var contents = spec.contents;
            var uuid = spec.uuid ? spec.uuid : Math.uuid();
            var timestamp = spec.timestamp ? spec.timestamp : null;
            var signingPeer = spec.signingPeer ? spec.signingPeer : null;
            this.base(
                {
                    contents: contents,
                    decryptedContents: null,
                    verifiedResults: null,
                    uuid: uuid,
                    timestamp: timestamp,
                    signingPeer: signingPeer
                }
            );
        },
        
        getUUID: function() {
            return this.uuid;
        },
        
        getTimestamp: function() {
            return this.timestamp;
        },
        
        isPlaintext: function() {
            return this.contents.type === 'plaintext';
        },
        
        isEncrypted: function() {
            return this.contents.type === 'encrypted';
        },
        
        isSignedOnly: function() {
            return this.contents.type === 'signed';
        },
        
        isSigned: function() {
            return this.isSignedOnly() || this.isSignedAndDecrypted();
        },
        
        isSignedAndDecrypted: function() {
            return this.isDecrypted() && this.decryptedContents.type === 'signed';
        },
        
        isDecrypted: function() {
            return this.isEncrypted() && this.decryptedContents;
        },
        
        isVerified: function() {
            return this.verifiedResults;
        },
        
        serialize: function() {
            return {
                blob: net_dushin_foundation.Serialization.serialize(
                    {
                        contents: this.contents,
                        signingPeer: this.signingPeer ? this.signingPeer.toPeerObject() : null
                    }
                ),
                uuid: this.uuid
            };
        },
        
        verify: function(peers, contents) {
            var signingPeer = this.signingPeer;
            var peerId = signingPeer.toPeerObject().id;
            var result = signingPeer.verifier.verify(contents);
            if (result.status) {;
                this.verifiedResults = {peer: signingPeer, value: result.value}
            } else {
                console.log("Signed message failed validation!!!");
            }
            /*
            var peer = net_dushin_foundation.Lists.find(
                function(peer) {
                    return (peer.toPeerObject().id === peerId);
                },
                peers
            );
            if (peer && peer.isTrusted) {
            }


            var that = this;
            var results = net_dushin_foundation.Lists.mapFirst(
                function(peer) {
                    try {
                        var result = peer.verifier.verify(contents);
                        if (result.status) {
                            return {peer: peer, value: result.value};
                        } else {
                            return false;
                        }
                    } catch (e) {
                        return false;
                    }
                },
                peers
            );
            if (results === null) {
                console.log("No peers signed the message!");
            } else {
                this.verifiedResults = results;
            }
            */
        },
        
        tryDecrypt: function(identity) {
            var decryptor = identity.decryptor;
            try {
                var obj = decryptor.decrypt(this.contents);
                this.decryptedContents = obj;
            } catch (e) {
                console.log("decryption failed");
            }
        },
        
        toHtml: function(
            trusted, plaintext, encrypted, decrypted, unsigned, verified, unverified
        ) {
            var prefix = "";
            var contents;
            if (!this.isEncrypted() && !this.isSigned()) { // plaintext
                prefix += this.img(plaintext);
                prefix += this.img(unsigned);
                contents = this.renderContentsHtml(this.contents);
            } else if (!this.isEncrypted() && this.isSigned() && !this.isVerified()) { // plaintext signed unverified
                prefix += this.img(plaintext);
                prefix += this.img(unverified);
                contents = "...";
            } else if (!this.isEncrypted() && this.isSigned() && this.isVerified()) { // plaintext signed verified
                prefix += this.img(plaintext);
                prefix += this.signingPeer.isTrusted ? this.img(trusted) : this.img(verified);
                contents = this.renderContentsHtml(this.verifiedResults.value);
            } else if (this.isEncrypted() && !this.isDecrypted()) { // undecrypted
                prefix += this.img(encrypted);
                contents = "...";
            } else if (this.isDecrypted() && !this.isSigned()) { // decrypted and not signed
                prefix += this.img(decrypted);
                prefix += this.img(unsigned);
                contents = this.renderContentsHtml(this.decryptedContents);
            } else if (this.isDecrypted() && this.isSigned() && this.isVerified()) { // decrypted, signed, and verified
                prefix += this.img(decrypted);
                prefix += this.signingPeer.isTrusted ? this.img(trusted) : this.img(verified);
                contents = this.renderContentsHtml(this.verifiedResults.value);
            } else if (this.isDecrypted() && !this.isVerified()) { // decrypted, signed, and unverified
                prefix += this.img(decrypted);
                prefix += this.img(unverified);
                contents = "...";
            }
            return "<p>" + prefix + contents + "</p>";
        },
        
        img: function(path) {
            return "<img src=\"" + path + "\"/> ";
        },
        
        renderContentsHtml: function(c) {
            return "<b>" + c.from + ":</b> " + c.messageText;
        }
    }
);

Lethe.Message.parse = function(spec) {
    var messageObject = spec.messageObject;
    var peers = spec.peers;
    var deblob = net_dushin_foundation.Serialization.deserialize(messageObject.blob);
    var contents = deblob.contents;
    var signingPeer = deblob.signingPeer ? Lethe.Peer.parse(deblob.signingPeer) : null;
    var knownPeer = signingPeer ? net_dushin_foundation.Lists.find(
        function(peer) {
            return peer.toPeerObject().id === signingPeer.toPeerObject().id
        },
        peers
    ) : null;
    return new Lethe.Message(
        {
            contents: contents, 
            uuid: messageObject.uuid, 
            timestamp: messageObject.timestamp,
            signingPeer: knownPeer ? knownPeer : signingPeer
        }
    );
};
