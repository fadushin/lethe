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

import net.dushin.lethe.messaging.common.jaxb.JaxbSerialization;
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

    private static final java.util.Map<javax.xml.namespace.QName, Class>
    QNAME_TYPE_MAP = new java.util.HashMap<javax.xml.namespace.QName, Class>();
    static {
        QNAME_TYPE_MAP.put(Constants.PLAINTEXT_MESSAGE_QNAME, PlaintextMessage.class);
        QNAME_TYPE_MAP.put(Constants.SIGNED_MESSAGE_QNAME, SignedMessage.class);
    }
    

    /**
     * Serialize a jaxb element in the specified package into a byte array.
     */
    protected static byte[]
    serialize(
        final Package pkg,
        final Object jaxbelement
    ) {
        return JaxbSerialization.serialize(pkg, jaxbelement);
    }
    
    /**
     * Deserialize a serialized structure from a serialized structure.
     */
    protected static Object
    deserialize(
        final Package pkg,
        final byte[] data
    ) {
        return JaxbSerialization.deserialize(pkg, data, QNAME_TYPE_MAP);
    }
    
    /**
     * placate checkstyle
     */
    protected final void
    dummy() {
        // complete
    }
}
