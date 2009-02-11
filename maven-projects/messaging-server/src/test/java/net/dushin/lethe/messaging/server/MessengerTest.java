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
import org.apache.cxf.testutil.common.AbstractBusClientServerTestBase;

public class MessengerTest extends AbstractBusClientServerTestBase {

    private static final java.net.URL WSDL_LOC;
    static {
        java.net.URL tmp = null;
        try {
            tmp = new java.net.URL(
                "http://localhost:18066/MessengerService/SOAPPort?wsdl"
            );
        } catch (final Exception e) {
            tmp = null;
        }
        WSDL_LOC = tmp;
    }
    
    private static final javax.xml.namespace.QName MESSENGER_SERVICE_QNAME =
        new javax.xml.namespace.QName(
            "http://lethe.dushin.net/messaging/server",
            "MessengerService"
        );
    
    private static final javax.xml.namespace.QName MESSENGER_SOAP_PORT_QNAME =
        new javax.xml.namespace.QName(
            "http://lethe.dushin.net/messaging/server",
            "SOAPPort"
        );

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
    testMessender() throws Exception {
        
        try {
            final javax.xml.ws.Service svc = 
                javax.xml.ws.Service.create(
                    WSDL_LOC,
                    MESSENGER_SERVICE_QNAME
                );
            final net.dushin.lethe.messaging.interfaces.Messenger messenger = svc.getPort(
                MESSENGER_SOAP_PORT_QNAME,
                net.dushin.lethe.messaging.interfaces.Messenger.class
            );
            //
            // Check to see that the channel is empty
            //
            assertSame(messenger.getMessages("foo", 0).getItem().size(), 0);
            //
            // post a message to the channel, and then check to see it's there
            //
            messenger.postMessage(
                "foo",
                createContents("bar")
            );
            MessageList foo = messenger.getMessages("foo", 0);
            assertSame(foo.getItem().size(), 1);
            assertSame(foo.getItem().get(0).getOrdinal(), 0);
            assertEquals(foo.getItem().get(0).getMessage().getMsg(), "bar");
            //
            // Check the logic of get
            //
            assertSame(messenger.getMessages("foo", 1).getItem().size(), 0);
            assertSame(messenger.getMessages("gnu", 0).getItem().size(), 0);
            //
            // Post another message, and check that it arrived, as well
            //
            messenger.postMessage(
                "foo",
                createContents("bar2")
            );
            foo = messenger.getMessages("foo", 0);
            assertSame(foo.getItem().size(), 2);
            assertSame(foo.getItem().get(0).getOrdinal(), 0);
            assertEquals(foo.getItem().get(0).getMessage().getMsg(), "bar");
            assertSame(foo.getItem().get(1).getOrdinal(), 1);
            assertEquals(foo.getItem().get(1).getMessage().getMsg(), "bar2");
            //
        } catch (final Exception e) {
            e.printStackTrace();
            throw e;
        }
    }
    
    private static Contents
    createContents(final String msg) {
        final Contents ret = new Contents();
        ret.setDescriptor("java.lang.String");
        ret.setMsg(msg);
        return ret;
    }
}
