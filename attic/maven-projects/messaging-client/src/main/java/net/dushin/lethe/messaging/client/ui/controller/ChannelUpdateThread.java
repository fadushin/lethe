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

import net.dushin.lethe.messaging.common.collections.Pair;
import net.dushin.lethe.messaging.common.log.LogUtil;
import net.dushin.lethe.messaging.interfaces.Message;
import net.dushin.lethe.messaging.interfaces.MessageList;
import net.dushin.lethe.messaging.interfaces.Messenger;
import net.dushin.lethe.messaging.interfaces.PeerList;

/**
 * This thread keeps the client side channel model as
 * current as it can, within a given window.
 */
class ChannelUpdateThread extends Thread {
    
    /**
     * The Logger instance to use for this type
     */
    private static final java.util.logging.Logger LOGGER =
        java.util.logging.Logger.getLogger(
            LetheController.class.getName()
        );

    /**
     * The default wait period for this background thread
     */
    private static final long DEFAULT_WAIT = 5000;
    
    /**
     * The channel with which this thread is associated
     */
    private final ChannelModel channel;
    
    /**
     * The connection source, from which the connection is obtained.
     */
    private final ConnectionSource connectionSource;
    
    /**
     * The identity source, from which the current client identity is obtained.
     */
    private final IdentitySource identitySource;
    
    /**
     * the entity to notify, when a new message is received.
     */
    private final MessageChangeListener messageChangeListener;
    
    /**
     * the entity to notify, when the list of peers changes.
     */
    private final PeerChangeListener peerChangeListener;
    
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
    
    ChannelUpdateThread(
        final ChannelModel channel,
        final ConnectionSource connectionSource,
        final IdentitySource identitySource,
        final MessageChangeListener messageChangeListener,
        final PeerChangeListener peerChangeListener
    ) {
        this.channel = channel;
        this.connectionSource = connectionSource;
        this.identitySource = identitySource;
        this.messageChangeListener = messageChangeListener;
        this.peerChangeListener = peerChangeListener;
    }
    
    public void
    run() {
        while (true) {
            if (halt) {
                LogUtil.logInfo(
                    LOGGER,
                    "MessageChangeThread request to halt; bye bye..." 
                );
                return;
            }
            try {
                final Messenger messenger = connectionSource.getConnection().
                    getMessenger();
                //
                // ping the channel, with the current client identity
                //
                messenger.hello(channel.getChannelId(), toPeerStruct(identitySource.getIdentity()));
                //
                // Get the list of peers on the channel; notify the listener
                // if there is anything new.
                //
                final PeerList peers = messenger.getPeers(channel.getChannelId());
                final Pair<java.util.List<Peer>, java.util.List<Peer>> delta =
                    this.channel.reconcilePeers(peers.getItem());
                if (!delta.getFirst().isEmpty() || !delta.getSecond().isEmpty()) {
                    this.peerChangeListener.peerChanged(delta);
                }
                //
                // Get the current list of messages; notify the message listener
                // if there are any new messages
                //
                final String since =
                    this.rawMessages.size() > 0
                        ? this.rawMessages.get(
                            this.rawMessages.size() - 1
                        ).getMessage().getUuid()
                        : "";
                LogUtil.logInfo(
                    LOGGER,
                    "Last message UUID received: \"{0}\"; polling server...", since 
                );
                final MessageList messages = 
                    messenger.getMessages(channel.getChannelId(), since);
                final java.util.List<Message> msgs = messages.getItem();
                if (msgs.size() > 0 || notifyChange) {
                    LogUtil.logInfo(
                        LOGGER,
                        "Un update is required because we received a positive number "
                        + "of messages back ({0}) or we were notified of a change "
                        + "({1}), or both", msgs.size(), notifyChange
                    );
                    this.rawMessages.addAll(msgs);
                    final java.util.List<ReceivedMessage> receivedMessages =
                        this.channel.receiveMessages(this.rawMessages);
                    this.messageChangeListener.messageChanged(receivedMessages);
                    notifyChange = false;
                } else {
                    LogUtil.logInfo(LOGGER, "No messages to update.");
                }
            } catch (final Exception e) {
                LogUtil.logException(
                    LOGGER, 
                    e, 
                    "An exception occurred updating channel." 
                );
                e.printStackTrace();
            } finally {
                synchronized (changeMonitor) {
                    try {
                        LogUtil.logInfo(LOGGER, "Sleeping {0}ms...", DEFAULT_WAIT);
                        changeMonitor.wait(DEFAULT_WAIT);
                    } catch (final InterruptedException e) {
                        // ignore
                    }
                }
            }
        }
    }
    
    //
    // protected operations
    //

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
    
    //
    // private operations
    //
    
    private net.dushin.lethe.messaging.interfaces.Peer toPeerStruct(Peer peer) {
        final net.dushin.lethe.messaging.interfaces.Peer ret =
            new net.dushin.lethe.messaging.interfaces.Peer();
        ret.setName(peer.getName());
        ret.setEncoded(peer.toString());
        return ret;
    }
}
