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
package net.dushin.lethe.messaging.interfaces;

/**
 * The following constants are used for serialization and deserialization
 * of JAXB types.
 */
public final class Constants {
    
    /**
     * The Messaging XML namespace
     */
    public static final String MESSAGING_NS = 
        "http://lethe.dushin.net/messaging/interfaces";
    
    //
    // Message service constants
    //
    
    /**
     * Message Service QName
     */
    public static final javax.xml.namespace.QName MESSAGE_SERVICE_QNAME =
        new javax.xml.namespace.QName(
            MESSAGING_NS,
            "MessengerService"
        );
    
    /**
     * Message Service QName
     */
    public static final javax.xml.namespace.QName MESSAGE_PORT_QNAME =
        new javax.xml.namespace.QName(
            MESSAGING_NS,
            "MessengerPort"
        );
    
    public static final String MESSAGE_SERVICE_URL_CONTEXT =
        MESSAGE_SERVICE_QNAME.getLocalPart() + "/" + MESSAGE_PORT_QNAME.getLocalPart();
    
    public static final String MESSAGE_CHANNEL_URL_CONTEXT_PREFIX =
        MESSAGE_SERVICE_QNAME.getLocalPart() + "/Channel-";
    
    //
    // Message type constants
    //
    
    /**
     * PlaintextMessage QName
     */
    public static final javax.xml.namespace.QName PLAINTEXT_MESSAGE_QNAME =
        new javax.xml.namespace.QName(
            MESSAGING_NS,
            "PlaintextMessage"
        );
    
    /**
     * SignedMessage QName
     */
    public static final javax.xml.namespace.QName SIGNED_MESSAGE_QNAME =
        new javax.xml.namespace.QName(
            MESSAGING_NS,
            "SignedMessage"
        );
    
    /**
     * EncryptedKey QName
     */
    public static final javax.xml.namespace.QName ENCRYPTED_KEY_QNAME =
        new javax.xml.namespace.QName(
            MESSAGING_NS,
            "EncryptedKey"
        );

    private
    Constants() {
        // complete
    }
}

