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

import net.dushin.lethe.messaging.interfaces.keys.PublicKeyType;

/**
 *
 */
public class KeyHelperTest extends org.junit.Assert {
    
    private static final String ALICE_PW = "alice";
    private static final java.security.KeyPair ALICE;
    static {
        java.security.KeyPair tmp = null;
        try {
            tmp = new KeyPairGenerator(512).generateKeyPair(ALICE_PW);
        } catch (final Exception e) {
            e.printStackTrace();
        }
        ALICE = tmp;
    }

    @org.junit.Test
    public void
    testKeyHelper() {
        //
        // Try serialization; not much negative to test
        //
        final String str = KeyHelper.toString("alice", ALICE.getPublic());
        assertNotNull(str);
        //
        // parse the string back into a PublicKeyType and test the bits
        // we can
        //
        final PublicKeyType key = KeyHelper.parse(str);
        assertNotNull(key);
        assertEquals(key.getName(), "alice");
        //
        // parse the key data and check the results are the same
        //
        final java.security.PublicKey pub = KeyHelper.getPublicKey(key);
        assertNotNull(pub);
        assertTrue(publicKeysEqual(ALICE.getPublic(), pub));
        
        //
        // TODO add alot more negative tests
        //
    }
    
    private static boolean
    publicKeysEqual(
        final java.security.PublicKey pub1,
        final java.security.PublicKey pub2
    ) {
        assert pub1 instanceof java.security.interfaces.RSAPublicKey;
        final java.security.interfaces.RSAPublicKey rsa1 =
            (java.security.interfaces.RSAPublicKey) pub1;
        assert pub2 instanceof java.security.interfaces.RSAPublicKey;
        final java.security.interfaces.RSAPublicKey rsa2 =
            (java.security.interfaces.RSAPublicKey) pub2;
        return rsa1.getModulus().equals(rsa2.getModulus())
            && rsa1.getPublicExponent().equals(rsa2.getPublicExponent());
    }
}
