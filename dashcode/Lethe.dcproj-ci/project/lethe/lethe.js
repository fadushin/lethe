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

    TAG_GENERATING: "generating..",

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
            
            setTmpIdentity: function(identity) {
                this.getModel().setValueForKeyPath(identity, "content.tmp.id");
            },
            
            regenerateTmpIdentityKey: function(progress) {
                var identity = this.getTmpIdentity();
                identity.setValueForKeyPath(
                    Lethe.TAG_GENERATING,
                    "pubKey"
                );
                setTimeout(
                    function() {
                        identity.regenerate(progress);
                    },
                    100
                );
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
            
            getTrustedPeers: function() {
                return this.getModel().valueForKeyPath("content.trustedPeers");
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
                backend.join(
                    channelName, obj,
                    function(result, error) {
                        if (!error) {
                            //
                            // Create the channel and add it to the list of channels
                            //
                            var channel = new Lethe.Channel(channelName, backend);
                            channels.addObject(channel);
                        } else {
                            console.error(error);
                        }
                    }
                );
            },
            
            leaveChannel: function(channel) {
                var identity = this.getIdentity()
                var channels = this.getChannels();
                var channelName = channel.valueForKeyPath('name');
                var peerId = identity.toPeerObject().id;
                //
                // Signal the update function to stop
                //
                channel.setValueForKeyPath(false, 'running');
                //
                // remove the channel from the back end
                //
                backend.leave(
                    channelName, peerId,
                    function(result, error) {
                        if (!error) {
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
                        } else {
                            console.error(error);
                        }
                    }
                );
            },
            
            updateIdentity: function() {
                var lethe = this;
                var identity = lethe.getIdentity();
                var oldIdentityPeerId = identity.getPeer() ? identity.getPeer().toPeerObject().id : null;
                var tmpIdentity = lethe.getTmpIdentity();
                identity.assign(tmpIdentity);
                //
                // Store the identity in persistent storage
                //
                net_dushin_foundation.Async.exec(
                    {
                        f: function() {
                            lethe.setStoredIdentity(identity);
                        }
                    }
                );
                //
                // Leave then rejoin each channel
                //
                var channels = lethe.getChannels();
                net_dushin_foundation.Lists.applyAsync(
                    function(channel) {
                        var channelName = channel.getName();
                        if (oldIdentityPeerId) {
                            backend.leave(
                                channelName, oldIdentityPeerId, 
                                function(result, error) {
                                    if (error) {
                                        console.error(error);
                                    }
                                }
                            );
                        }
                        var peerObject = identity.toPeerObject();
                        backend.join(
                            channelName, peerObject,
                            function(result, error) {
                                if (error) {
                                    console.error(error);
                                }
                            }
                        );
                    },
                    channels
                );
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
                //
                // (possibly) sign and encrypt the message, and send it.  Do this as asynchronously as possible.
                //
                net_dushin_foundation.Async.exec({
                    f: function(contents) {
                        return signMessage ? identity.signer.sign(contents) : contents;
                    },
                    args: contents,
                    resultCallback: function(contents) {
                        var recipientEncryptors = net_dushin_foundation.Lists.filterMap(
                            function(peer) {
                                var encryptTo = peer.valueForKeyPath('encryptTo');
                                return encryptTo ? peer.encryptor : false; 
                            },
                            peers
                        );
                        //
                        // Add the identity (peer), if the message is at all encrypted
                        //
                        if (recipientEncryptors.length > 0) {
                            recipientEncryptors.push(identity.getPeer().encryptor);
                        }
                        net_dushin_foundation.Async.exec({
                            f: function(contents) {
                                if (recipientEncryptors.length > 0) {
                                    contents = bulkEncryptor.encrypt(contents, recipientEncryptors);
                                }
                                return new Lethe.Message(
                                    {
                                        contents: contents, 
                                        signingPeer: signMessage ? identity.getPeer() : null
                                    }
                                );
                            },
                            args: contents,
                            resultCallback: function(message) {
                                backend.sendMessage(
                                    channelName, message.serialize(),
                                    function(result, error) {
                                        if (!error) {
                                            /*
                                            net_dushin_foundation.Async.exec(
                                                {f: function() { channel.updateAll(); }}
                                            );
                                            */
                                        } else {
                                            console.error(error);
                                        }
                                    }
                                );
                            }
                        });
                    }
                });
            }
        };
    }
};

Lethe.KVO.deserialize = function(str) {
    return net_dushin_foundation.Serialization.deserialize(str);
};


Lethe.init = function() {
    // var lethe = Lethe.create(Lethe.serverBackend.create());
    var lethe = Lethe.create(Lethe.dummyBackend);
    lethe.setIdentity(new Lethe.Identity("", "", ""));
    lethe.setTmpIdentity(new Lethe.Identity("", "", ""));
    lethe.getModel().setValueForKeyPath([], "content.channels");
    this.instance = lethe;
    return lethe;
};


Lethe.serverBackend = {
    
    jsonrpc: JSOlait.imprt("jsonrpc"),
    
    methods: ["get_channels", "get_peers", "join", "leave", "get_messages", "get_messages_since", "post_message", "update"],
    
    create: function(spec) {
    
        spec = spec ? spec : {};

        var serviceURL = spec.serviceURL ? spec.serviceURL : "../rs/rpc";

        var proxy = new this.jsonrpc.ServiceProxy(serviceURL, this.methods);

        var pinger = new this.jsonrpc.JSONRPCMethod(serviceURL, "ping");
    
    
        return {
            join: function(channelName, peer, callback) {
                return proxy.join(channelName, peer, callback);
            },
            
            leave: function(channelName, peerId, callback) {
                return proxy.leave(channelName, peerId, callback);
            },
            
            sendMessage: function(channelName, message, callback) {
                return proxy.post_message(channelName, message, callback);
            },
            
            /*
            ping: function(channelName, peer) {
                return pinger.notify(channelName, peer);
            },

            getPeers: function(channelName, peerNames) {
                return proxy.get_peers(channelName, peerNames);
            },
            
            getAllMessages: function(channelName) {
                return proxy.get_messages(channelName);
            },
            
            getMessagesSince: function(channelName, timestamp) {
                return proxy.get_messages_since(channelName, timestamp);
            },
            */
            
            update: function(channelName, obj, callback) {
                return proxy.update(channelName, obj, callback);
            }
        };
    }
};

Lethe.dummyBackend = {
            
    dummyChannels: {
        'test': {
            peers: { 
                /* Alice */
                'yGq9p3Je9ewx21FW60X4nav8ONU=': {
                    id: 'yGq9p3Je9ewx21FW60X4nav8ONU=', 
                    blob: 'eyJuYW1lIjogIkFsaWNlIiwgInB1YktleSI6ICJBNkgyemNyR0JDdTVqT3VkS3pkbUQrd1ArUDFGOGU0Ylh2WW5NNnFqRFFEWEtKVU9CdVlIdDQ0Y2pWNU5ncW54VlQyOXNMdC9HSjJ6UEZSVHR5NXdIcVc1YjF0dlVYNUFodU5YV0ZTYk1Tc1N3WFZEWTI0MjYrWjlUT3J3b3UrbWZIYjZURjg4WlRGL2JuWUIzV2h5K1E9PSJ9'
                },
                /* Bob */
                'U979yYhpcqqpRQauwZFye/1HBtk="': {
                    id: 'U979yYhpcqqpRQauwZFye/1HBtk="', 
                    blob: 'eyJuYW1lIjogIkJvYiIsICJwdWJLZXkiOiAiSnYvU1p3ajAzNXBYdGxwQ042TzhnQktRUThNb2FLZTk4dGsrcVVLTHkwQjNDRCtCWXpSeURGVVF4RlBkNklGL09Fd3N2Uk1RelF6ckkyWmRVUWNCRnpoVG9rOTdWOFZ5Rk1IeXJnY1ZyVTZDZ3FUaGNPVG9KVHJVVzlwdG9KeHBESGRaZHFXM1ZveU5FbWFrdVJzQU9nPT0ifQ=='
                },
            }, 
            messages: [
                {
                    uuid: '361B2C17-4457-4AC8-B40E-EF72F9B0D84A',
                    timestamp: '0',
                    blob: 'eyJjb250ZW50cyI6IHsidHlwZSI6ICJzaWduZWQiLCAic2VyaWFsaXplZCI6ICJleUowZVhCbElqb2dJbkJzWVdsdWRHVjRkQ0lzSUNKbWNtOXRJam9nSWtGc2FXTmxJaXdnSW0xbGMzTmhaMlZVWlhoMElqb2dJa2hwSUVKdllpRWlmUT09IiwgImhhc2giOiAieVhRc0FKSTRsY2RSS2xERjVid1l1WEpPUUo0PSIsICJzaWduYXR1cmUiOiAiWnUrdlg1VHNZTUNTNTN2Q2JJRmtpdEIrcjRINEFTNEpQNkNYQUxLSEFFK3JvUDR2dHV0cTU5RG82OHJ3THl3WkVjNmsxVmxlWHY1LzZNSk1GYWZFTFE9PSJ9LCAic2lnbmluZ1BlZXIiOiB7ImlkIjogInlHcTlwM0plOWV3eDIxRlc2MFg0bmF2OE9OVT0iLCAiYmxvYiI6ICJleUp1WVcxbElqb2dJa0ZzYVdObElpd2dJbkIxWWt0bGVTSTZJQ0pCTmtneWVtTnlSMEpEZFRWcVQzVmtTM3BrYlVRcmQxQXJVREZHT0dVMFlsaDJXVzVOTm5GcVJGRkVXRXRLVlU5Q2RWbElkRFEwWTJwV05VNW5jVzU0VmxReU9YTk1kQzlIU2pKNlVFWlNWSFI1TlhkSWNWYzFZakYwZGxWWU5VRm9kVTVZVjBaVFlrMVRjMU4zV0ZaRVdUSTBNallyV2psVVQzSjNiM1VyYldaSVlqWlVSamc0V2xSR0wySnVXVUl6VjJoNUsxRTlQU0o5In19'
                }
            ]
        }
    },
    
    join: function(channelName, peer, callback) {
        var dummyChannel = this.dummyChannels[channelName];
        if (!dummyChannel) {
            dummyChannel = {
                peers: {},
                messages: []
            };
            this.dummyChannels[channelName] = dummyChannel;
        }
        dummyChannel.peers[peer.id] = peer;
        if (callback) {
            callback(null, null);
        }
    },
    
    leave: function(channelName, peerId, callback) {
        delete this.dummyChannels[channelName].peers[peerId];
        if (callback) {
            callback(null, null);
        }
    },
    
    sendMessage: function(channelName, message, callback) {
        message.timestamp = this.dummyChannels[channelName].messages.length;
        this.dummyChannels[channelName].messages.push(message);
        if (callback) {
            callback(null, null);
        }
    },
            
    update: function(channelName, obj, callback) {
        var peerUpdate = this.getPeers(channelName, obj.update_peers);
        var messageUpdate = obj.update_messages === "all" ?
            this.getAllMessages(channelName) :
            this.getMessagesSince(channelName, obj.update_messages);
        var result = {
            peer_update: peerUpdate,
            message_update: messageUpdate
        };
        if (callback) {
            callback(result, null);
        } else {
            return result;
        }
    },
    
    

    ping: function(channelName, peer) {
        // no-op
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
            if (dummyChannel.peers.hasOwnProperty(dummyPeerName)) {
                if (!net_dushin_foundation.Lists.contains(dummyPeerName, peerNames)) {
                    ret.add.push(dummyChannel.peers[dummyPeerName]);
                }
            }
        }
        return ret;
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

