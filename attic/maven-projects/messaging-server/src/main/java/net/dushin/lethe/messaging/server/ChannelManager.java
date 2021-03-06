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

import javax.xml.ws.Endpoint;

import net.dushin.lethe.messaging.common.collections.Pair;
import net.dushin.lethe.messaging.server.config.ChannelConfigType;
import net.dushin.lethe.messaging.server.config.MessagingServerConfigType;

class ChannelManager {

    private final java.util.Map<String, Pair<Channel, Endpoint>> channelMap =
        new java.util.HashMap<String, Pair<Channel, Endpoint>>();

    private final MessagingServerConfigType serverConfig;

    ChannelManager(
        final MessagingServerConfigType serverConfig
    ) {
        this.serverConfig = serverConfig;
        new SweeperThread(this).start();
    }
    
    Channel
    getOrCreateChannel(
        final ChannelConfigType channelConfig,
        final String channelID
    ) {
        synchronized (channelMap) {
            Pair<Channel, Endpoint> ret = channelMap.get(channelID);
            if (ret == null) {
                final Channel channel = new Channel(channelConfig, channelID);
                ret = new Pair<Channel, Endpoint>(
                    channel, 
                    null
                );
                channelMap.put(channelID, ret);
            }
            return ret.getFirst();
        }
    }
    
    java.util.Map<String, Pair<Channel, Endpoint>>
    getChannelMap() {
        return this.channelMap;
    }
    
    MessagingServerConfigType
    getMessagingServerConfig() {
        return this.serverConfig;
    }
}
