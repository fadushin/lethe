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

import javax.xml.ws.wsaddressing.W3CEndpointReference;

import net.dushin.lethe.messaging.client.ws.ChannelClientProxy;
import net.dushin.lethe.messaging.client.ws.MessengerClientProxy;
import net.dushin.lethe.messaging.interfaces.Channel;
import net.dushin.lethe.messaging.interfaces.Messenger;

/**
 * An Connection is a represenation of the connection between
 * a lethe client and lethe server. It is initialized with
 * with the host and port
 */
public class Connection {
    
    public static final String DEFAULT_HOST = "localhost";
    
    public static final short DEFAULT_PORT = 8080;
    
    public static final Connection LOCALHOST = new Connection();
    
    private final String host;
    
    private final short port;
    
    private final boolean connected;
    
    private final java.util.Map<String, ChannelClientProxy> channelProxyMap =
        new java.util.HashMap<String, ChannelClientProxy>();
    
    /**
     * The jax-ws proxy (wrapper) to the server
     */
    private MessengerClientProxy proxy;
    
    //
    // lifecycle
    //
    
    public
    Connection() {
        this(DEFAULT_HOST, DEFAULT_PORT);
    }
    
    public
    Connection(
        final String host,
        final short port
    ) {
        this.host = host;
        this.port = port;
        //
        //
        //
        boolean tmp = false;
        try {
            getProxy().ping();
            tmp = true;
        } catch (final Exception e) {
            // keep false
        } finally {
            this.connected = tmp;
        }
    }
    
    //
    // public operations
    //
    
    public String
    getHost() {
        return this.host;
    }
    
    public short
    getPort() {
        return this.port;
    }
    
    public boolean
    getConnected() {
        return this.connected;
    }
    
    public ChannelClientProxy
    getChannelClientProxy(final String channelId) throws Exception {
        synchronized (channelProxyMap) {
            ChannelClientProxy ret = this.channelProxyMap.get(channelId);
            if (ret == null) {
                final W3CEndpointReference ref = this.getProxy().getChannel(channelId);
                ret = new ChannelClientProxy(ref.getPort(Channel.class));
                this.channelProxyMap.put(channelId, ret);
            }
            return ret;
        }
    }
    
    public void
    removeChannelClientProxy(final String channelId) {
        synchronized (channelProxyMap) {
            this.channelProxyMap.remove(channelId);
        }
    }
    
    //
    // internal operations
    //
    
    Messenger
    getProxy() {
        synchronized (this) {
            try {
                if (this.proxy == null) {
                    this.proxy = new MessengerClientProxy(getURL());
                }
                return this.proxy.getProxy();
            } catch (final Exception e) {
                // log it?
                throw new RuntimeException("Unable to resolve proxy", e);
            }
        }
    }
    
    private java.net.URL
    getURL() {
        try {
            return new java.net.URL(
                "http://" + host + ':' + port + "/MessengerService/SOAPPort?wsdl"
            );
        } catch (final java.net.MalformedURLException e) {
            assert false;
            throw new RuntimeException("This code is unreachable", e);
        }
    }
}
