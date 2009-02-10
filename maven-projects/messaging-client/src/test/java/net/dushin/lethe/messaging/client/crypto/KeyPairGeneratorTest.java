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


/**
 *
 */
public class KeyPairGeneratorTest extends org.junit.Assert {
    
    private static final String PASS1 = "To be, or not to be";
    private static final String PASS2 = "That is the question";

    /**
     * @throws      Exception if an error occurred
     */
    @org.junit.Test
    public final void
    testKeyPairGenerator() throws Exception {
        try {
            final java.security.KeyPair pair5121 =
                new KeyPairGenerator(512).generateKeyPair(PASS1);
            final java.security.KeyPair pair5122 =
                new KeyPairGenerator(512).generateKeyPair(PASS2);
            //
            //
            //
            assertNotNull(
                pair5121
            );
            assertNotNull(
                pair5122
            );
            assertNotSame(
                pair5121,
                new KeyPairGenerator(512).generateKeyPair(PASS1)
            );
            assertNotSame(
                new KeyPairGenerator(512).generateKeyPair(PASS1),
                pair5121
            );
            assertTrue(
                keyPairsEqual(
                    new KeyPairGenerator(512).generateKeyPair(PASS1),
                    pair5121
                )
            );
            assertTrue(
                keyPairsEqual(
                    pair5121,
                    new KeyPairGenerator(512).generateKeyPair(PASS1)
                )
            );
            assertFalse(
                keyPairsEqual(
                    pair5121,
                    pair5122
                )
            );
        } catch (final Exception e) {
            e.printStackTrace();
            org.junit.Assert.fail("unexpected exception");
        }
    }
    
    private boolean
    keyPairsEqual(
        final java.security.KeyPair kp1,
        final java.security.KeyPair kp2
    ) {
        return publicKeysEqual(kp1.getPublic(), kp2.getPublic())
            && privateKeysEqual(kp1.getPrivate(), kp2.getPrivate());
    }
    
    private boolean
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
    
    private boolean
    privateKeysEqual(
        final java.security.PrivateKey priv1,
        final java.security.PrivateKey priv2
    ) {
        assert priv1 instanceof java.security.interfaces.RSAPrivateKey;
        final java.security.interfaces.RSAPrivateKey rsa1 =
            (java.security.interfaces.RSAPrivateKey) priv1;
        assert priv2 instanceof java.security.interfaces.RSAPrivateKey;
        final java.security.interfaces.RSAPrivateKey rsa2 =
            (java.security.interfaces.RSAPrivateKey) priv2;
        return rsa1.getModulus().equals(rsa2.getModulus())
            && rsa1.getPrivateExponent().equals(rsa2.getPrivateExponent());
    }
}
