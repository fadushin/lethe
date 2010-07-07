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

var Lethe = {

    KVO: Class.create(
        DC.KVO,
        {
            constructor: function(object) {
                var name, value;
                for (name in object) {
                    if (object.hasOwnProperty(name) && typeof object[name] != 'function') {
                        this.setValueForKeyPath(object[name], name);
                        this[name] = object[name];
                    }
                }
            },
            
            serialize: function(obj) {
                return net_dushin_foundation.Serialization.serialize(obj);
            }
        }
    ),
    
    create: function(backend) {
    
        var that = this;
        
        backend = backend ? backend : {
            
            dummyChannels: {messages: []},
            
            ordinal: 0,
            
            join: function(channelName, peer) {
                var dummyChannel = this.dummyChannels[channelName];
                if (!dummyChannel) {
                    dummyChannel = {peers: {}, messages: []};
                    this.dummyChannels[channelName] = dummyChannel;
                }
                dummyChannel.peers[peer.id] = peer;
            },

            getPeers: function(channelName, peerNames) {
                var dummyChannel = this.dummyChannels[channelName];
                var ret = {add: [], remove: []};
                if (!dummyChannel) {
                    return ret;
                }
                var i
                for (i = 0;  i < peerNames.length;  ++i) {
                    var peerName = peerNames[i];
                    var dummyPeer = dummyChannel.peers[peerName];
                    if (!dummyPeer) {
                        ret.remove.push(peerName);
                    }
                }
                for (dummyPeerName in dummyChannel.peers) {
                    if (!net_dushin_foundation.Lists.contains(dummyPeerName, peerNames)) {
                        ret.add.push(dummyChannel.peers[dummyPeerName]);
                    }
                }
                return ret;
            },
            
            leave: function(channelName) {
                delete this.dummyChannels[channelName];
            },
            
            sendMessage: function(channelName, message) {
                message.timestamp = this.ordinal++;
                this.dummyChannels[channelName].messages.push(message);
            },
            
            getAllMessages: function(channelName) {
                return this.dummyChannels[channelName].messages;
            },
            
            getMessagesSince: function(channelName, timestamp) {
                return net_dushin_foundation.Lists.filter(
                    function(message) {
                        return message.timestamp > timestamp;
                    },
                    this.dummyChannels[channelName].messages
                );
            }
        };
        
        var BROWSER_STORAGE_WARNING = "Your browser does not support localStorage."
            + "  Your identity will not be preserved across sessions of your web browser."
            + "  Try an HTML5-compliant broswer, such as one based on WebKit.";

        var setStoredItem = function(key, value) {
            if (localStorage) {
                return localStorage.setItem(key, value);
            } else {
                console.info(BROWSER_STORAGE_WARNING);
                return null;
            }
        };

        var getStoredItem = function(key) {
            if (localStorage) {
                return localStorage.getItem(key);
            } else {
                console.info(BROWSER_STORAGE_WARNING);
                return null;
            }
        };
        
        var bulkEncryptor = net_dushin_crypto.EncryptorFactory.createBulkEncryptor();
        
        return {
        
            getModel: function() {
                return dashcode.getDataSource("lethe");
            },
            
            getIdentity: function() {
                return this.getModel().valueForKeyPath("content.identity");
            },
            
            setIdentity: function(identity) {
                this.getModel().setValueForKeyPath(identity, "content.identity");
            },
            
            getTmpIdentity: function() {
                return this.getModel().valueForKeyPath("content.tmp.id");
            },
            
            getChannels: function() {
                return this.getModel().valueForKeyPath("content.channels");
            },
            
            getChannel: function(channelName) {
                var channels = this.getChannels();
                return net_dushin_foundation.Lists.find(
                    function(channels) {
                        var nom = channels.getName();
                        return nom === channelName;
                    },
                    channels
                );
            },
            
            getSelectedChannel: function() {
                var channelList = dashcode.getDataSource("ChannelList");
                var channel = channelList.valueForKeyPath("selection");
                return channel;
            },
    
            getStoredIdentity: function() {
                var item = getStoredItem("net.dushin.lethe.identity");
                if (item) {
                    try {
                        return Lethe.Identity.deserialize(item);
                    } catch (e) {
                        alert(e);
                        return null;
                    }
                } else {
                    return null;
                }
            },
    
            setStoredIdentity: function(identity) {
                var str = identity.serialize();
                setStoredItem("net.dushin.lethe.identity", str);
            },
            
            joinChannel: function(channelName) {
                var identity = this.getIdentity();
                var channels = this.getChannels();
                //
                // Check to ensure that the channel is not already created
                //
                var i;
                for (i = 0;  i < channels.length;  ++i) {
                    if (channels[i].getName() === channelName) {
                        alert("The channel name must be unique.  Please try again.");
                        return;
                    }
                }
                //
                // Join the channel (on the back end)
                //
                var obj = identity.toPeerObject();
                backend.join(channelName, obj);
                //
                // Create the channel and add it to the list of channels
                //
                var channel = new Lethe.Channel(channelName, backend);
                channels.addObject(channel);
            },
            
            leaveChannel: function(channel) {
                var channels = this.getChannels();
                var channelName = channel.valueForKeyPath('name');
                //
                // Signal the update function to stop
                //
                channel.setValueForKeyPath(false, 'running');
                // clearInterval(channel.updateId)
                //
                // remove the channel from the back end
                //
                backend.leave(channelName);
                //
                // remove it from the model
                //
                var index = net_dushin_foundation.Lists.indexOf(
                    function(chan) {
                        return chan.getName() === channelName
                    },
                    channels
                );
                if (index !== -1) {
                    channels.removeObjectAtIndex(index);
                }
            },
            
            updateIdentity: function(oldName, identity) {
                var channels = this.getChannels();
                var i;
                for (i = 0;  i < channels.length;  ++i) {
                    var channel = channels[i];
                    var channelName = channel.getName();
                    if (oldName) {
                        backend.leave(channelName);
                    }
                    var obj = identity.toPeerObject();
                    backend.join(channelName, obj);
                    channel.updatePeers();
                }
            },
            
            sendMessage: function(channelName, messageText) {
                var identity = this.getIdentity();
                var contents = {
                    type: 'plaintext',
                    from: identity.getName(),
                    messageText: messageText
                };
                var channel = this.getChannel(channelName);
                var peers = channel.getPeers();
                var messages = channel.valueForKeyPath('messages');
                var signMessage = channel.valueForKeyPath('signMessages');
                if (signMessage) {
                    contents = identity.signer.sign(contents);
                }
                var recipientEncryptors = net_dushin_foundation.Lists.filterMap(
                    function(peer) {
                        return peer.encryptTo ? peer.encryptor : false; 
                    },
                    peers
                );
                if (recipientEncryptors.length > 0) {
                    contents = bulkEncryptor.encrypt(contents, recipientEncryptors);
                }
                var message = new Lethe.Message(contents);
                
                backend.sendMessage(channelName, message.serialize());
                
                channel.updateMessages();
            }
        };
    }
};

Lethe.KVO.deserialize = function(str) {
    return net_dushin_foundation.Serialization.deserialize(str);
};

Lethe.Identity = Class.create(
    Lethe.KVO,
    {
        constructor: function(name, privKey, pubKey) {
            this.base(
                {
                    name: name, 
                    privKey: privKey, 
                    pubKey: pubKey,
                    //
                    // derived properties
                    //
                    signer: privKey ? net_dushin_crypto.SignerFactory.createSigner(
                        {privateKey: privKey}
                    ) : null,
                    decryptor: privKey ? net_dushin_crypto.DecryptorFactory.createDecryptor(
                        {privateKey: privKey}
                    ) : null
                }
            );
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
        },
        
        getName: function() {
            return this.name;
        },
        
        setName: function(nom) {
            this.name = nom;
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
            var signer = this.valueForKeyPath("signer");
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

Lethe.Channel = Class.create(
    Lethe.KVO,
    {
        constructor: function(name, backend) {
            var that = this;
            this.base(
                {
                    name: name,
                    backend: backend,
                    signMessages: true,
                    peers: [], 
                    messages: [],
                    running: true,
                    updateId: setInterval(
                        function() {
                            that.update();
                        },
                        5000
                    )
                }
            );
            setTimeout(
                that.update(),
                0
            );
        },
        
        getName: function() {
            return this.name;
        },
        
        getPeers: function() {
            return this.peers;
        },
        
        getMessages: function() {
            return this.messages;
        },
        
        update: function() {
            this.updatePeers();
            // this.updateMessages();
        },

        updatePeers: function() {
            var channelName = this.name;
            if (!this.running) {
                console.log("Shutting down update function for channel " + channelName);
                clearInterval(this.intervalId);
                return;
            }
            console.log("Updating channel " + channelName + "...");
            //
            // Get the peer ids out of the ppers
            //
            var peers = this.valueForKeyPath('peers');
            var peerIds = net_dushin_foundation.Lists.map(
                function(peer) {
                    return peer.valueForKeyPath('peerId');
                },
                peers
            );
            //
            // Call getPeers on the back end, for the channel name, and
            // with the current list of peer IDs.  This will return a
            // list of the peer IDs that should be removed, and a list of
            // peers that should be added.
            //
            var peerUpdate = this.valueForKeyPath('backend').getPeers(channelName, peerIds);
            //
            // Remove the peers that should be removed from the model,
            //
            var i;
            for (i = 0;  i < peerUpdate.remove.length;  ++i) {
                var peerId = peerUpdate.remove[i];
                var peer = net_dushin_foundation.Lists.find(
                    function(peer) {
                        return peer.valueForKeyPath('peerId') === peerId;
                    },
                    peers
                );
                if (peer != null) {
                    peers.removeObject(peer);
                }
            }
            //
            // and add the ones that should be added.
            //
            for (i = 0;  i < peerUpdate.add.length;  ++i) {
                var parsedPeer = Lethe.Peer.parse(peerUpdate.add[i]);
                peers.addObject(parsedPeer);
            }
        },
        
        updateMessages: function() {
            var identity = Lethe.instance.getIdentity();
            var peers = this.getPeers();
            var messages = this.getMessages();
            var backend = this.valueForKeyPath('backend');
            var channelName = this.name;
            var newMessages;
            if (messages.length == 0) {
                newMessages = backend.getAllMessages(channelName);
            } else {
                var lastMessage = messages[messages.length - 1];
                newMessages = backend.getMessagesSince(channelName, lastMessage.getTimestamp());
            }
            var that = this;
            var processedMessages = net_dushin_foundation.Lists.map(
                function(newMessage) {
                    var message = Lethe.Message.parse(newMessage);
                    // TODO decrypt/verify
                    
                    if (message.isPlaintext()) {
                        return message;
                    }
                    if (message.isSignedOnly()) {
                        message.verify(peers, message.contents);
                        return message;
                    }
                    if (message.isEncrypted()) {
                        message.tryDecrypt(identity);
                    }
                    if (message.isSignedAndDecrypted()) {
                        message.verify(peers, message.decryptedContents);
                    }
                    return message;
                },
                newMessages
            );
            net_dushin_foundation.Lists.apply(
                function(message) {
                    messages.addObject(message);
                },
                processedMessages
            );
        },
        
        shutdown: function() {
            this.running = false;
        }
    }
);

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
        }/*,
        
        getPeerId: function() {
            return this.peerId;
        }*/
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


Lethe.Message = Class.create(
    Lethe.KVO,
    {
        constructor: function(contents, uuid, timestamp) {
            this.base(
                {
                    contents: contents,
                    decryptedContents: null,
                    verifiedResults: null,
                    uuid: uuid ? uuid : Math.uuid(),
                    timestamp: timestamp ? timestamp : null
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
                blob: net_dushin_foundation.Serialization.serialize(this.contents),
                uuid: this.uuid
            };
        },
        
        verify: function(peers, contents) {
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
        
        toString: function() {
            var prefix = "";
            var contents;
            /*
            if (this.isPlaintext()) {
                prefix += "PLAINTEXT::";
                contents = this.renderContents(this.contents);
            }
            */
            if (this.isEncrypted()) {
                if (this.isDecrypted()) {
                    prefix += "DECRYPTED::";
                } else {
                    prefix += "ENCRYPTED::";
                    contents = "...";
                }
            } else {
                prefix += "UNENCRYPTED::";
            }
            if (this.isSigned()) {
                if (this.isVerified()) {
                    var results = this.verifiedResults;
                    prefix += "SIGNATURE-VERIFIED::";
                        /*
                        + net_dushin_crypto.KeyUtil.keyFingerprint(results.peer.pubKey)
                        + ")::";
                        */ 
                    contents = this.renderContents(results.value);
                } else {
                    prefix += "SIGNATURE-UNVERIFIED::";
                    // this won't work
                    contents = "...";
                }
            } else {
                prefix += "UNSIGNED::";
                contents = this.renderContents(
                    this.isDecrypted() ? this.decryptedContents : this.contents
                );
            }
            return prefix + contents;
        },
        
        renderContents: function(c) {
            return c.from + ': ' + c.messageText;
        }
    }
);

Lethe.Message.parse = function(obj) {
    var contents = net_dushin_foundation.Serialization.deserialize(obj.blob);
    return new Lethe.Message(contents, obj.uuid, obj.timestamp);
};

Lethe.init = function() {
    var lethe = Lethe.create();
    lethe.setIdentity(new Lethe.Identity("", "", ""));
    this.instance = lethe;
    return lethe;
};


Lethe.serverBackend = {
    
    jsonrpc: JSOlait.imprt("jsonrpc"),
    
    methods: ["get_channels", "get_peers", "join", "leave", "get_messages", "get_messages_since", "post_message"],
    
    create: function(spec) {
    
        spec = spec ? spec : {};

        var serviceURL = spec.serviceURL ? spec.serviceURL : "rs/rpc";

        var proxy = new jsonrpc.ServiceProxy(serviceURL, methods);

        var pinger = new jsonrpc.JSONRPCMethod(serviceURL, "ping");
    
    
        return {
            join: function(channelName, peer) {
                return proxy.join(channelName, peer);
            },

            getPeers: function(channelName, peerNames) {
                return proxy.get_peers(channelName, peerNames);
            },
            
            leave: function(channelName) {
                return proxy.leave(channelName);
            },
            
            sendMessage: function(channelName, message) {
                return proxy.post_message(channelName, message);
            },
            
            getAllMessages: function(channelName) {
                return proxy.get_messages(channelName);
            },
            
            getMessagesSince: function(channelName, timestamp) {
                return proxy.get_messages_since(channelName, timestamp);
            }
        };
    }
};
