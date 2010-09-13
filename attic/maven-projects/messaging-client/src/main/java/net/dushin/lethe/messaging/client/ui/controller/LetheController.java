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

/**
 * This class encapsulates the state of a Lethe client.  It holds
 * the identity information for the user (his or her key pair), together
 * with the information for all of the known peers of the user.
 *
 * The controller is used to send and receive messages through the
 * lethe system.  Each message is associated with a channel, each channel
 * is associated with a thread, which receives messages sent to the channel.
 */
public class LetheController implements ConnectionSource, IdentitySource {
    
    //
    // data
    //
    
    /**
     * the connection to the server
     */
    private Connection connection = Connection.LOCALHOST;

    /**
     * The identity associated with this controller
     */
    private Identity identity = Identity.ANONYMOUS;
    
    /**
     * the collection of peers associated with this controller
     
    private final java.util.List<Peer> peers =
        new java.util.ArrayList<Peer>();*/
    
    /**
     * a map from channel ids to threads.  Each thread is responsible for
     * receiving (in a polling manner) messages delivered to the designated
     * channel.
     */
    private final java.util.Map<String, ChannelModel> channelMap =
        new java.util.HashMap<String, ChannelModel>();
    
    //
    // Lifecycle
    //
    
    public
    LetheController() {
    }
    
    public
    LetheController(
        final Connection connection
    ) {
        this.connection = connection;
    }
    
    //
    // public operations
    //
    
    public Connection
    getConnection() {
        return this.connection;
    }
    
    public void
    setConnection(
        final Connection connection
    ) {
        this.connection = connection;
        notifyChannels();
    }
    
    private void
    notifyChannels() {
        synchronized (channelMap) {
            for (ChannelModel channel : channelMap.values()) {
                channel.notifyChange();
            }
        }
    }
    
    /**
     * @return      the Identity associated with this controller
     */
    public Identity
    getIdentity() {
        return this.identity;
    }
    
    /**
     * Set the identity associated with this controller.
     *
     * @param       identity
     *              the identity to set
     */
    public void
    setIdentity(
        final Identity identity
    ) {
        this.identity = identity;
        notifyChannels();
    }
    
    /**
     * Remove the message change listener associated with the specified
     * channel.
     */
    public void
    removeMessageChangedListener(
        final String channelId
    ) {
        synchronized (channelMap) {
            ChannelModel channel = channelMap.get(channelId);
            if (channel != null) {
                channel.notifyHalt();
                channelMap.remove(channelId);
            }
        }
    }

    public ChannelModel 
    createChannel(
        final String channelId, 
        final MessageChangeListener messageChangeListener,
        final PeerChangeListener peerChangeListener
    ) {
        return new ChannelModel(channelId, this, this, messageChangeListener, peerChangeListener);
    }
    
    //
    // private operations
    //
}
