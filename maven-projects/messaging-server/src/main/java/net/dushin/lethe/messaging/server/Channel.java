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
import net.dushin.lethe.messaging.interfaces.Message;
import net.dushin.lethe.messaging.interfaces.MessageList;
import net.dushin.lethe.messaging.server.config.ChannelConfigType;

class Channel {

    private final ChannelConfigType channelConfig;
    
    private final String id;
    
    private final MessageList messages =
        new MessageList();
    
    private int totalMessages;
    
    private long lastTouched = Timestamp.currentms();

    Channel(
        final ChannelConfigType channelConfig,
        final String id
    ) {
        this.channelConfig = 
            channelConfig == null ? new ChannelConfigType() : channelConfig;
        this.id = id;
    }

    MessageList
    getMessages(
        final java.lang.String since
    ) {
        synchronized (messages) {
            return getSince(since);
        }
    }

    void
    postMessage(
        final Contents message
    ) {
        synchronized (messages) {
            final java.util.List<Message> msgs = this.messages.getItem();
            if (msgs.size() == this.channelConfig.getMaxMessages()) {
                msgs.remove(0);
            }
            final Message msg = new Message();
            msg.setTimestampMs(Timestamp.currentms());
            msg.setMessage(message);
            msgs.add(msg);
            this.totalMessages++;
        }
    }
    
    private MessageList
    getSince(
        final java.lang.String since
    ) {
        final MessageList ret = new MessageList();
        final java.util.List<Message> src = messages.getItem();
        final java.util.List<Message> dst = ret.getItem();
        boolean found = false;
        for (Message msg : src) {
            if (found) {
                dst.add(msg);
            } else if (since.equals(msg.getMessage().getUuid())) {
                found = true;
            }
        }
        return found ? ret : this.messages;
    }
    
    void
    sweepMessages() {
        final java.util.List<Message> src = messages.getItem();
        final long currentms = Timestamp.currentms();
        for (Message msg : src) {
            final long deltasecs = (currentms - msg.getTimestampMs()) / 1000;
            if (deltasecs > this.channelConfig.getMessageTimeoutSecs()) {
                src.remove(msg);
                continue;
            } else {
                return;
            }
        }
    }
    
    long
    getLastTouched() {
        return this.lastTouched;
    }
    
    void
    setLastTouched(
        final long lastTouched
    ) {
        this.lastTouched = lastTouched;
    }
}
