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

/**
 *
 */
class MessageChangeThread extends Thread {
    
    private static final long DEFAULT_WAIT = 5000;
    
    /**
     * The channel with which this thread is associated
     */
    private final String channel;
    
    /**
     * The controller, which is used to manage message
     * receipt.
     */
    private final LetheController controller;
    
    /**
     * the entity to notify, when a new message is received.
     */
    private final MessageChangeListener listener;
    
    /**
     * the list of received messages on this channel
     */
    private final java.util.List<Message> rawMessages =
        new java.util.ArrayList<Message>();

    /**
     * whether this thread should continue operating
     */
    private boolean halt;
    
    /**
     * whether some state has changed that requires a refresh
     */
    private boolean notifyChange;
    
    /**
     * Monitor on state change.  This thread will wait on this
     * * monitor between polls, but will wake up if it is notified
     * of a state change.
     */
    private final Object changeMonitor = new Object();
    
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
                final String since =
                    this.rawMessages.size() > 0
                        ? this.rawMessages.get(
                            this.rawMessages.size() - 1
                        ).getMessage().getUuid()
                        : "";
                final MessageList messages = 
                    controller.getConnection().getProxy().getMessages(channel, since);
                final java.util.List<Message> msgs = messages.getItem();
                if (msgs.size() > 0 || notifyChange) {
                    this.rawMessages.addAll(msgs);
                    final java.util.List<ReceivedMessage> receivedMessages =
                        this.controller.receiveMessages(this.rawMessages);
                    this.listener.messageChanged(receivedMessages);
                    notifyChange = false;
                }
            } catch (final Exception e) {
                // log it?
                e.printStackTrace();
            } finally {
                synchronized (changeMonitor) {
                    try {
                        changeMonitor.wait(DEFAULT_WAIT);
                    } catch (final InterruptedException e) {
                        // ignore
                    }
                }
            }
        }
    }
    
    /**
     * Tell this thread to halt
     */
    void
    notifyHalt() {
        this.halt = true;
        synchronized (this.changeMonitor) {
            this.changeMonitor.notifyAll();
        }
    }
    
    /**
     * Tell the thread that some state has changed, and
     * a new list of messages is needed.
     */
    void
    notifyChange() {
        this.notifyChange = true;
        synchronized (this.changeMonitor) {
            this.changeMonitor.notifyAll();
        }
    }
}
