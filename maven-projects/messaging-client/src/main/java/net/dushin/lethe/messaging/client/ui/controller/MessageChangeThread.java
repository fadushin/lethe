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
package net.dushin.lethe.messaging.client.ui.controller;

import net.dushin.lethe.messaging.interfaces.Message;
import net.dushin.lethe.messaging.interfaces.MessageList;

public class MessageChangeThread extends Thread {

    private final String channel;
    private final LetheController controller;
    private final MessageChangeListener listener;
    
    //private final java.util.List<ReceivedMessage> receivedMessages =
    //    new java.util.ArrayList<ReceivedMessage>();
    
    private final java.util.List<Message> rawMessages =
        new java.util.ArrayList<Message>();

    private boolean halt;
    private int since;
    private boolean notifyChange;
    
    MessageChangeThread(
        final String channel,
        final LetheController controller,
        final MessageChangeListener listener
    ) {
        this.channel = channel;
        this.controller = controller;
        this.listener = listener;
    }
    
    public void
    run() {
        while (true) {
            if (halt) {
                return;
            }
            try {
                final MessageList messages = controller.getProxy().getMessages(channel, since);
                if (messages.getItem().size() > 0 || notifyChange) {
                    since += messages.getItem().size();
                    this.rawMessages.addAll(messages.getItem());
                    final java.util.List<ReceivedMessage> receivedMessages =
                        this.controller.receiveMessages(this.rawMessages);
                    this.listener.messageChanged(receivedMessages);
                    notifyChange = false;
                }
                Thread.sleep(1000);
            } catch (final Exception e) {
                // log it?
                e.printStackTrace();
            }
        }
    }
    
    void
    notifyHalt() {
        this.halt = true;
    }
    
    void
    notifyChange() {
        this.notifyChange = true;
    }
}
