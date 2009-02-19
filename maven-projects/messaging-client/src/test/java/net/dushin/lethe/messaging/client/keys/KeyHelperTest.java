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

import net.dushin.lethe.messaging.client.crypto.KeyPairGenerator;
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

    private static final String BOB_PW = "bob";
    private static final java.security.KeyPair BOB;
    static {
        java.security.KeyPair tmp = null;
        try {
            tmp = new KeyPairGenerator(512).generateKeyPair(BOB_PW);
        } catch (final Exception e) {
            e.printStackTrace();
        }
        BOB = tmp;
    }

    @org.junit.Test
    public void
    testKeyHelper() {
        final String str = KeyHelper.toString("alice", ALICE.getPublic());
        System.out.println(str);
        final PublicKeyType key = KeyHelper.parse(str);
        final java.security.PublicKey pub = KeyHelper.getPublicKey(key);
        assertNotNull(pub);
    }
}
