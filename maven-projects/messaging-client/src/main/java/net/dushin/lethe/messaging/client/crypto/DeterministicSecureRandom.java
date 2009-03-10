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

public class DeterministicSecureRandom extends java.security.SecureRandomSpi {
    
    final java.util.Random rand = new java.util.Random();
    
    public
    DeterministicSecureRandom() {
    }
    
    protected void 
    engineSetSeed(
        final byte[] seed
    ) {
        rand.setSeed(mosh(seed));
    }
    
    protected void 
    engineNextBytes(
        final byte[] bytes
    ) {
        rand.nextBytes(bytes);
    }
    
    protected byte[] 
    engineGenerateSeed(
        final int numBytes
    ) {
        throw new RuntimeException("unimplemented");
    }
    
    private static long
    mosh(
        final byte[] seed
    ) {
        long ret = 0;
        final int seedDiv8 = seed.length / 8;
        for (int i = 0;  i < seedDiv8;  ++i) {
            final int iTimes8 = i * 8;
            for (int j = 0;  j < 8;  ++j) {
                ret ^= seed[iTimes8 + j] << (j * 8);
            }
        }
        final int seedMod8 = seed.length % 8;
        final int seedDiv8Times8 = seedDiv8 * 8;
        for (int j = 0;  j < seedMod8;  ++j) {
            ret ^= seed[seedDiv8Times8 + j] << (j * 8);
        }
        return ret;
    }
}
