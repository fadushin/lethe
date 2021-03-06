/**
 * Copyright (c) dushin.net
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of dushin.net nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package net.dushin.lethe.messaging.server;

import net.dushin.lethe.messaging.interfaces.Contents;
import net.dushin.lethe.messaging.interfaces.MessageList;
import net.dushin.lethe.messaging.interfaces.Peer;
import net.dushin.lethe.messaging.interfaces.PeerList;
import net.dushin.lethe.messaging.server.config.MessagingServerConfigType;

public class Messenger
    implements net.dushin.lethe.messaging.interfaces.Messenger {
    
    private final ChannelManager channelMgr;
    
    private final MessagingServerConfigType serverConfig;

    public
    Messenger(
        final MessagingServerConfigType serverConfig
    ) {
        this.serverConfig = serverConfig;
        this.channelMgr = new ChannelManager(serverConfig);
    }
    
    public void
    ping() {
        // log it, or something
    }

    public void hello(String channelId, Peer peer) {
        channelMgr.getOrCreateChannel(
            this.serverConfig.getChannelConfig(), 
            channelId
        ).hello(peer);
    }

    public void goodbye(String channelId, Peer peer) {
        channelMgr.getOrCreateChannel(
            this.serverConfig.getChannelConfig(), 
            channelId
        ).bye(peer);
    }

    public MessageList getMessages(String channelId, String since) {
        return channelMgr.getOrCreateChannel(
            this.serverConfig.getChannelConfig(), 
            channelId
        ).getMessages(since);
    }

    public PeerList getPeers(String channelId) {
        return channelMgr.getOrCreateChannel(
            this.serverConfig.getChannelConfig(), 
            channelId
        ).getPeers();
    }

    public void postMessage(String channelId, Contents message) {
        channelMgr.getOrCreateChannel(
            this.serverConfig.getChannelConfig(), 
            channelId
        ).postMessage(message);
    }
}
