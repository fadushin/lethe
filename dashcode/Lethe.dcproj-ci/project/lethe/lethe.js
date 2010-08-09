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
            
            regenerateTmpIdentityKey: function() {
                var identity = this.getTmpIdentity();
                identity.setValueForKeyPath(
                    Lethe.TAG_GENERATING,
                    "pubKey"
                );
                setTimeout(
                    function() {
                        identity.regenerate();
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
                var obj = identity.peer;
                backend.join(channelName, obj);
                //
                // Create the channel and add it to the list of channels
                //
                var channel = new Lethe.Channel(channelName, backend);
                channels.addObject(channel);
            },
            
            leaveChannel: function(channel) {
                var identity = this.getIdentity()
                var channels = this.getChannels();
                var channelName = channel.valueForKeyPath('name');
                var peerId = identity.peer.id;
                //
                // Signal the update function to stop
                //
                channel.setValueForKeyPath(false, 'running');
                //
                // remove the channel from the back end
                //
                backend.leave(channelName, peerId);
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
                var lethe = this;
                var identity = lethe.getIdentity();
                var oldName = identity.getName();
                setTimeout(
                    function() {
                        var tmpIdentity = lethe.getTmpIdentity();
                        identity.assign(tmpIdentity);
                        lethe.setStoredIdentity(identity);
                        //
                        var channels = lethe.getChannels();
                        var i;
                        for (i = 0;  i < channels.length;  ++i) {
                            var channel = channels[i];
                            var channelName = channel.getName();
                            if (oldName) {
                                backend.leave(channelName);
                            }
                            var obj = identity.peer;
                            backend.join(channelName, obj);
                            channel.updatePeers();
                        }
                    },
                    200
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
                                return peer.encryptTo ? peer.encryptor : false; 
                            },
                            peers
                        );
                        net_dushin_foundation.Async.exec({
                            f: function(contents) {
                                if (recipientEncryptors.length > 0) {
                                    contents = bulkEncryptor.encrypt(contents, recipientEncryptors);
                                }
                                return new Lethe.Message(contents);
                            },
                            args: contents,
                            resultCallback: function(message) {
                                backend.sendMessage(channelName, message.serialize());
                                net_dushin_foundation.Async.exec(
                                    {f: function() { channel.updateMessages(); }}
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
    
    methods: ["get_channels", "get_peers", "join", "leave", "get_messages", "get_messages_since", "post_message"],
    
    create: function(spec) {
    
        spec = spec ? spec : {};

        var serviceURL = spec.serviceURL ? spec.serviceURL : "../rs/rpc";

        var proxy = new this.jsonrpc.ServiceProxy(serviceURL, this.methods);

        var pinger = new this.jsonrpc.JSONRPCMethod(serviceURL, "ping");
    
    
        return {
            join: function(channelName, peer) {
                return proxy.join(channelName, peer);
            },

            ping: function(channelName, peer) {
                return pinger.notify(channelName, peer);
            },

            getPeers: function(channelName, peerNames) {
                return proxy.get_peers(channelName, peerNames);
            },
            
            leave: function(channelName, peerId) {
                return proxy.leave(channelName, peerId);
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

Lethe.dummyBackend = {
            
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
            if (!net_dushin_foundation.Lists.contains(dummyPeerName, peerNames)) {
                ret.add.push(dummyChannel.peers[dummyPeerName]);
            }
        }
        return ret;
    },
    
    leave: function(channelName, peerId) {
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
