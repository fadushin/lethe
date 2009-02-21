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
package net.dushin.lethe.messaging.client.keys;

import net.dushin.lethe.messaging.client.jaxb.JaxbSerialization;
import net.dushin.lethe.messaging.interfaces.keys.ObjectFactory;
import net.dushin.lethe.messaging.interfaces.keys.PublicKeyType;
import net.dushin.lethe.messaging.interfaces.keys.RSAPublicKeyType;
import org.apache.cxf.common.util.Base64Utility;

/**
 * Base type for ojects that require serializationa and deserialization
 * operations, such as for marshalling and unmarshalling encrypted or signed
 * data.
 *
 * Operations on this class are static.
 */
public abstract class KeyHelper {
    
    private static final String BEGIN_LETHE_PUBLIC_KEY =
        "--- BEGIN LETHE PUBLIC KEY ---";

    private static final String END_LETHE_PUBLIC_KEY =
        "--- END LETHE PUBLIC KEY ---";

    private static final String NL =
        System.getProperty("line.separator");
    
    private static final int COL_WRAP = 72;
    
    private static final ObjectFactory KEYS_OBJ_FACTORY = new ObjectFactory();
    
    
    private
    KeyHelper() {
    }
    
    public static PublicKeyType
    createPublicKeyType(
        final String name,
        final java.security.PublicKey key
    ) {
        final PublicKeyType ret = new PublicKeyType();
        ret.setName(name);
        final RSAPublicKeyType pubkey = new RSAPublicKeyType();
        pubkey.setEncoded(
            key.getEncoded()
        );
        ret.setAny(KEYS_OBJ_FACTORY.createRSAPublicKey(pubkey));
        return ret;
    }
    
    
    public static String
    toString(
        final PublicKeyType key
    ) {
        final byte[] serialized = serialize(
            KEYS_OBJ_FACTORY.createPublicKey(key)
        );
        return
            BEGIN_LETHE_PUBLIC_KEY + NL
            + encode(serialized)
            + END_LETHE_PUBLIC_KEY;
    }
    
    
    public static String
    toString(
        final String name,
        final java.security.PublicKey key
    ) {
        return toString(createPublicKeyType(name, key));
    }
    
    public static PublicKeyType
    parse(
        final String pkg
    ) {
        int startidx = pkg.indexOf(BEGIN_LETHE_PUBLIC_KEY);
        if (startidx == -1) {
            throw new RuntimeException("Could not find start of key");
        }
        int endidx = pkg.indexOf(END_LETHE_PUBLIC_KEY);
        final String encoded = pkg.substring(
            startidx + BEGIN_LETHE_PUBLIC_KEY.length(), 
            endidx
        );
        byte[] serialized = null;
        try {
            serialized = Base64Utility.decode(encoded);
        } catch (final org.apache.cxf.common.util.Base64Exception e) {
            throw new RuntimeException("Error base64 decoding the data", e);
        }
        return deserialize(serialized);
    }
    
    public static java.security.PublicKey
    getPublicKey(
        final PublicKeyType key
    ) {
        Object any = key.getAny();
        byte[] encoded = null;
        if (any instanceof javax.xml.bind.JAXBElement) {
            javax.xml.bind.JAXBElement elt = (javax.xml.bind.JAXBElement) any;
            if (elt.getValue() instanceof RSAPublicKeyType) {
                RSAPublicKeyType pub = (RSAPublicKeyType) elt.getValue();
                encoded = pub.getEncoded();
            }
        } else if (any instanceof RSAPublicKeyType) {
            RSAPublicKeyType pub = (RSAPublicKeyType) any;
            encoded = pub.getEncoded();
        }
        try {
            final java.security.KeyFactory factory = 
                java.security.KeyFactory.getInstance("RSA");
            return factory.generatePublic(new java.security.spec.X509EncodedKeySpec(encoded));
        } catch (final java.security.NoSuchAlgorithmException e) {
            throw new RuntimeException("Error finding RSA algorithm", e);
        } catch (final java.security.spec.InvalidKeySpecException e) {
            throw new RuntimeException("Could not generate public key", e);
        }
    }
    
    //
    // internal operations
    //
    
    private static String
    encode(
        final byte[] data
    ) {
        final String encoded = Base64Utility.encode(data);
        final StringBuilder buf = new StringBuilder();
        final int n = encoded.length();
        for (int i = 0;  i < n / COL_WRAP;  ++i) {
            final int k = i * COL_WRAP;
            buf.append(encoded.substring(k, k + COL_WRAP));
            buf.append(NL);
        }
        buf.append(encoded.substring(COL_WRAP * (n / COL_WRAP), n));
        buf.append(NL);
        return buf.toString();
    }
    
    /**
     * Serialize a jaxb element in the specified package into a byte array.
     */
    private static byte[]
    serialize(
        final Object jaxbelement
    ) {
        return JaxbSerialization.serialize(PublicKeyType.class.getPackage(), jaxbelement);
    }
    
    /**
     * Deserialize a serialized structure from a serialized structure.
     */
    private static PublicKeyType
    deserialize(
        final byte[] data
    ) {
        return JaxbSerialization.deserialize(
            PublicKeyType.class.getPackage(), 
            data, 
            PublicKeyType.class
        );
    }
}
