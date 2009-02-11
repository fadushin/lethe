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

public class Channel {

    private final String id;
    
    private final MessageList messages =
        new MessageList();

    public
    Channel(
        final String id
    ) {
        this.id = id;
    }

    public MessageList
    getMessages(
        final int since
    ) {
        synchronized (messages) {
            if (since <= 0) {
                return messages;
            } else {
                return getSince(since);
            }
        }
    }

    public void
    postMessage(
        final Contents message
    ) {
        synchronized (messages) {
            final Message msg = new Message();
            msg.setOrdinal(messages.getItem().size());
            msg.setMessage(message);
            messages.getItem().add(msg);
        }
    }
    
    private MessageList
    getSince(
        final int since
    ) {
        final MessageList ret = new MessageList();
        final java.util.List<Message> src = messages.getItem();
        final java.util.List<Message> dst = ret.getItem();
        int i = 0;
        for (Message msg : src) {
            if (i >= since) {
                dst.add(msg);
            }
            ++i;
        }
        return ret;
    }
}
