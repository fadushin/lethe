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

import javax.xml.ws.wsaddressing.W3CEndpointReference;

import net.dushin.lethe.messaging.interfaces.Constants;
import net.dushin.lethe.messaging.interfaces.Contents;
import net.dushin.lethe.messaging.interfaces.MessageList;
import org.apache.cxf.testutil.common.AbstractBusClientServerTestBase;

public class MessengerTest extends AbstractBusClientServerTestBase {

    private static final java.net.URL WSDL_LOC;
    static {
        java.net.URL tmp = null;
        try {
            tmp = new java.net.URL(
                "http://localhost:8080/" + Constants.MESSAGE_SERVICE_URL_CONTEXT + "?wsdl"
            );
        } catch (final Exception e) {
            tmp = null;
        }
        WSDL_LOC = tmp;
    }

    private static final java.util.Map<String, String> EMPTY_MAP =
        java.util.Collections.emptyMap();
    
    @org.junit.BeforeClass
    public static void
    startServers() {
        assertTrue(
            "Server failed to launch",
            launchServer(
                BusServer.class, 
                EMPTY_MAP,
                new String[] {
                    "net/dushin/lethe/messaging/server/cxf-server.xml"
                },
                true
            )
        );
    }

    @org.junit.Test
    public final void
    testMessenger() throws Exception {
        
        try {
            final net.dushin.lethe.messaging.interfaces.Channel channel = getChannel("foo");
            //
            // Check to see that the channel is empty
            //
            assertSame(channel.getMessages("").getItem().size(), 0);
            //
            // post a message to the channel, and then check to see it's there
            //
            channel.postMessage(
                createContents("bar")
            );
            MessageList foo = channel.getMessages("");
            assertSame(foo.getItem().size(), 1);
            assertEquals(foo.getItem().get(0).getMessage().getMsg(), "bar");
            //
            // Check the logic of get
            //
            assertSame(
                channel.getMessages(
                    foo.getItem().get(0).getMessage().getUuid()
                ).getItem().size(), 
                0
            );
            assertSame(getChannel("gnu").getMessages("").getItem().size(), 0);
            //
            // Post another message, and check that it arrived, as well
            //
            channel.postMessage(
                createContents("bar2")
            );
            foo = channel.getMessages("");
            assertSame(foo.getItem().size(), 2);
            assertEquals(foo.getItem().get(0).getMessage().getMsg(), "bar");
            assertEquals(foo.getItem().get(1).getMessage().getMsg(), "bar2");
            //
        } catch (final Exception e) {
            e.printStackTrace();
            throw e;
        }
    }
    
    private net.dushin.lethe.messaging.interfaces.Channel getChannel(final String id) {
        final javax.xml.ws.Service svc = 
            javax.xml.ws.Service.create(
                WSDL_LOC,
                Constants.MESSAGE_SERVICE_QNAME
            );
        final net.dushin.lethe.messaging.interfaces.Messenger messenger = svc.getPort(
            Constants.MESSAGE_PORT_QNAME,
            net.dushin.lethe.messaging.interfaces.Messenger.class
        );
        final W3CEndpointReference ref = messenger.getChannel(id);
        return ref.getPort(net.dushin.lethe.messaging.interfaces.Channel.class);
    }

    private static Contents
    createContents(final String msg) {
        final Contents ret = new Contents();
        ret.setDescriptor("java.lang.String");
        ret.setUuid(java.util.UUID.randomUUID().toString());
        ret.setMsg(msg);
        return ret;
    }
}
