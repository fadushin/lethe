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
                    intervalId: setInterval(
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
        
        setMessages: function(messages) {
            var tmp = this.messages;
            this.messages = messages;
            return tmp;
        },
        
        checkRunning: function() {
            if (!this.running) {
                console.log("Shutting down update function for channel " + this.channelName);
                clearInterval(this.intervalId);
                return false;
            }
            return true;
        },
        
        update: function() {
            if (!this.checkRunning()) {
                return;
            }
            var that = this;
            /*
            net_dushin_foundation.Async.exec({f: function(){that.ping()}});
            net_dushin_foundation.Async.exec({f: function(){that.updatePeers()}});
            net_dushin_foundation.Async.exec({f: function(){that.updateMessages()}});
            */
            net_dushin_foundation.Async.exec({f: function(){that.updateAll()}});
        },
        
        updateAll: function() {
            //
            // Get the identity and channel name
            //
            var identity = Lethe.instance.getIdentity();
            var channelName = this.name;
            console.log("Updating channel " + channelName + "...");
            //
            // Get the peer ids out of the peers, and the messages
            //
            var peers = this.valueForKeyPath('peers');
            var peerIds = net_dushin_foundation.Lists.map(
                function(peer) {
                    return peer.valueForKeyPath('peerObject').id;
                },
                peers
            );
            var messages = this.getMessages();
            //
            // Call the back-end's update function
            //
            var backend = this.valueForKeyPath('backend');
            var since;
            if (messages.length == 0) {
                since = "all";
            } else {
                var lastMessage = messages[messages.length - 1];
                since = lastMessage.getTimestamp();
            }
            var channel = this;
            backend.update(
                channelName,
                {
                    ping: identity.getPeer().toPeerObject().id,
                    update_peers: peerIds,
                    update_messages: since
                },
                function(result, err) {
                    if (result) {
                        //
                        // Check for errors
                        //
                        if (result.error) {
                            if (result.error === "PEER_DOES_NOT_EXIST") {
                                //
                                // Join the channel (on the back end)
                                //
                                var obj = identity.toPeerObject();
                                backend.join(channelName, obj);
                            } else {
                                // unhandled
                                console.error("Unhandled error: " + result.error);
                            }
                            return;
                        }
                        var peerUpdate = result.peer_update;
                        var newMessages = result.message_update;
                        //
                        // Remove the peers that should be removed from the model,
                        //
                        net_dushin_foundation.Lists.applyAsync(
                            function(peerId) {
                                var peer = net_dushin_foundation.Lists.find(
                                    function(peer) {
                                        return peer.valueForKeyPath('peerObject').id === peerId;
                                    },
                                    peers
                                );
                                if (peer != null) {
                                    peers.removeObject(peer);
                                }
                            },
                            peerUpdate.remove
                        );
                        //
                        // and add the ones that should be added.
                        //
                        var identityPeerObject = identity.toPeerObject();
                        net_dushin_foundation.Lists.applyAsync(
                            function(addedPeer) {
                                var found = net_dushin_foundation.Lists.find(
                                    function(peer) {
                                        return peer.toPeerObject().id === addedPeer.id;
                                    },
                                    peers
                                );
                                if (found) {
                                    return;
                                }
                                try {
                                    var isTrusted = addedPeer.id === identityPeerObject.id;
                                    var parsedPeer = Lethe.Peer.parse(addedPeer, isTrusted);
                                    //
                                    // update the list of messages, if the isTrusted flag gets changed
                                    // (so that the messsages list gets re-rendered)
                                    //
                                    parsedPeer.addObserverForKeyPath(
                                        {
                                            trustedChanged: function(change){
                                                // TODO there needs to be a better way to do this...
                                                var tmp = channel.setMessages([]);
                                                channel.setMessages(tmp);
                                            }
                                        }, 
                                        'trustedChanged', 
                                        'isTrusted'
                                    );
                                    peers.addObject(parsedPeer);
                                } catch (e) {
                                    // console.log("An error occurred parsing a peer from the server:");
                                    console.error(e);
                                }
                            },
                            peerUpdate.add
                        );
                        //
                        // process the new messages
                        //
                        var processedMessages = net_dushin_foundation.Lists.applyAsync(
                            function(newMessage) {
                                var message = Lethe.Message.parse(
                                    {messageObject: newMessage, peers: peers}
                                );
                                //
                                // Skip the message, if it's already in the list (by UUID)
                                //
                                if (net_dushin_foundation.Lists.find(
                                    function(msg) {
                                        return message.getUUID() === msg.getUUID();
                                    },
                                    messages
                                )) {
                                    return;
                                }
                                if (message.isPlaintext()) {
                                    // no-op
                                } else {
                                    if (message.isSignedOnly()) {
                                        message.verify([identity.getPeer()], message.contents);
                                    } else {
                                        if (message.isEncrypted()) {
                                            message.tryDecrypt(identity);
                                        }
                                        if (message.isSignedAndDecrypted()) {
                                            message.verify([identity.getPeer()], message.decryptedContents);
                                        }
                                    }
                                }
                                messages.addObject(message);
                            },
                            newMessages
                        );
                    } else if (err) {
                        console.error(err);
                    }
                }
            );
        },
        
        /*
        ping: function() {
            var identity = Lethe.instance.getIdentity();
            var channelName = this.name;
            this.valueForKeyPath('backend').ping(channelName, identity.peer.id, function(){});
        },

        updatePeers: function() {
            var identity = Lethe.instance.getIdentity();
            var channelName = this.name;
            console.log("Updating channel " + channelName + "...");
            //
            // Get the peer ids out of the peers
            //
            var peers = this.valueForKeyPath('peers');
            var peerIds = net_dushin_foundation.Lists.map(
                function(peer) {
                    return peer.valueForKeyPath('peerObject').id;
                },
                peers
            );
            //
            // Call getPeers on the back end, for the channel name, and
            // with the current list of peer IDs.  This will return a
            // list of the peer IDs that should be removed, and a list of
            // peers that should be added.
            //
            var backend = this.valueForKeyPath('backend');
            var peerUpdate = backend.getPeers(channelName, peerIds);
            //
            // Remove the peers that should be removed from the model,
            //
            net_dushin_foundation.Lists.applyAsync(
                function(peerId) {
                    var peer = net_dushin_foundation.Lists.find(
                        function(peer) {
                            return peer.valueForKeyPath('peerObject').id === peerId;
                        },
                        peers
                    );
                    if (peer != null) {
                        peers.removeObject(peer);
                    }
                },
                peerUpdate.remove
            );
            //
            // and add the ones that should be added.
            //
            net_dushin_foundation.Lists.applyAsync(
                function(addedPeer) {
                    try {
                        var parsedPeer = Lethe.Peer.parse(addedPeer);
                        peers.addObject(parsedPeer);
                    } catch (e) {
                        // console.log("An error occurred parsing a peer from the server:");
                        console.error(e);
                    }
                },
                peerUpdate.add
            );
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
            var processedMessages = net_dushin_foundation.Lists.applyAsync(
                function(newMessage) {
                    var message = Lethe.Message.parse(newMessage);
                    if (message.isPlaintext()) {
                        // no-op
                    } else {
                        if (message.isSignedOnly()) {
                            message.verify(peers, message.contents);
                        } else {
                            if (message.isEncrypted()) {
                                message.tryDecrypt(identity);
                            }
                            if (message.isSignedAndDecrypted()) {
                                message.verify(peers, message.decryptedContents);
                            }
                        }
                    }
                    messages.addObject(message);
                },
                newMessages
            );
        },
        */
        
        shutdown: function() {
            this.running = false;
        }
    }
);
