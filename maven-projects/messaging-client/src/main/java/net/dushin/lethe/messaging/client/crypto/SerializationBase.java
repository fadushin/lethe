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
package net.dushin.lethe.messaging.client.crypto;

import net.dushin.lethe.messaging.interfaces.PlaintextMessage;

abstract class SerializationBase {

    protected byte[]
    serialize(
        final Package pkg,
        final Object jaxbelement
    ) {
        try {
            final javax.xml.bind.JAXBContext ctx =
                javax.xml.bind.JAXBContext.newInstance(
                    pkg.getName()
                );
            final javax.xml.bind.Marshaller marshaller = ctx.createMarshaller();
            final java.io.ByteArrayOutputStream os =
                new java.io.ByteArrayOutputStream();
            marshaller.marshal(
                jaxbelement, 
                os
            );
            return os.toByteArray();
        } catch (final Exception e) {
            throw new RuntimeException("Error marshalling " + jaxbelement, e);
        }
    }
    
    protected Object
    deserialize(
        final Package pkg,
        final byte[] data
    ) {
        try {
            final javax.xml.bind.JAXBContext ctx =
                javax.xml.bind.JAXBContext.newInstance(
                    pkg.getName()
                );
            final javax.xml.bind.Unmarshaller unmarshaller = ctx.createUnmarshaller();
            final java.io.ByteArrayInputStream is =
                new java.io.ByteArrayInputStream(data);
            return unmarshaller.unmarshal(
                new javax.xml.transform.stream.StreamSource(is), 
                PlaintextMessage.class
            );
        } catch (final Exception e) {
            throw new RuntimeException("Error unmarshalling " + data, e);
        }
    }
}
