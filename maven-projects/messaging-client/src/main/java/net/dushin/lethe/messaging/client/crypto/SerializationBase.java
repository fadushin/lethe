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

import net.dushin.lethe.messaging.interfaces.Constants;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

/**
 * Base type for ojects that require serializationa and deserialization
 * operations, such as for marshalling and unmarshalling encrypted or signed
 * data.
 *
 * Operations on this class are static.
 */
abstract class SerializationBase {
    
    /**
     * a map from package names to JAXBContext instances which can be used to
     * obtain JAXB marshallers and unmarshallers.
     */
    private static final java.util.Map<String, javax.xml.bind.JAXBContext> CONTEXT_MAP =
        new java.util.HashMap<String, javax.xml.bind.JAXBContext>();
    
    private static final javax.xml.parsers.DocumentBuilderFactory DOC_BUILDER_FACTORY = 
        javax.xml.parsers.DocumentBuilderFactory.newInstance();
    static {
        DOC_BUILDER_FACTORY.setNamespaceAware(true);        
    }

    /**
     * Serialize a jaxb element in the specified package into a byte array.
     */
    protected static byte[]
    serialize(
        final Package pkg,
        final Object jaxbelement
    ) {
        try {
            final javax.xml.bind.JAXBContext ctx =
                getJAXBContext(
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
    
    /**
     * Deserialize a serialized structure from a serialized structure.
     */
    protected static Object
    deserialize(
        final Package pkg,
        final byte[] data
    ) {
        try {
            final javax.xml.bind.JAXBContext ctx =
                getJAXBContext(
                    pkg.getName()
                );
            final javax.xml.bind.Unmarshaller unmarshaller = ctx.createUnmarshaller();
            final java.io.ByteArrayInputStream is =
                new java.io.ByteArrayInputStream(data);
            final javax.xml.parsers.DocumentBuilder builder = DOC_BUILDER_FACTORY.newDocumentBuilder();
            final org.w3c.dom.Document doc = builder.parse(is);
            final org.w3c.dom.Element root = doc.getDocumentElement();
            final javax.xml.namespace.QName qn =
                new javax.xml.namespace.QName(
                    root.getNamespaceURI(),
                    root.getLocalName()
                );
            Object obj = null;
            if (Constants.PLAINTEXT_MESSAGE_QNAME.equals(qn)) {
                obj = unmarshaller.unmarshal(
                    root, 
                    PlaintextMessage.class
                );                
            } else if (Constants.SIGNED_MESSAGE_QNAME.equals(qn)) {
                obj = unmarshaller.unmarshal(
                    doc, 
                    SignedMessage.class
                );
            }
            if (obj instanceof javax.xml.bind.JAXBElement) {
                return ((javax.xml.bind.JAXBElement) obj).getValue();
            } else {
                return obj;
            }
        } catch (final Exception e) {
            throw new RuntimeException("Error unmarshalling " + data, e);
        }
    }
    
    /**
     * @return      a cached JAXBContext for the specified package name,
     *              or a new one, if one has not been created.
     */
    private static javax.xml.bind.JAXBContext
    getJAXBContext(
        final String pkgname
    ) {
        synchronized (CONTEXT_MAP) {
            javax.xml.bind.JAXBContext ret = CONTEXT_MAP.get(pkgname);
            if (ret == null) {
                try {
                    ret = javax.xml.bind.JAXBContext.newInstance(
                        pkgname
                    );
                    CONTEXT_MAP.put(pkgname, ret);
                } catch (final Exception e) {
                    throw new RuntimeException("Error resolving " + pkgname, e);
                }
            }
            return ret;
        }
    }
    
    /**
     * placate checkstyle
     */
    protected final void
    dummy() {
        // complete
    }
}
