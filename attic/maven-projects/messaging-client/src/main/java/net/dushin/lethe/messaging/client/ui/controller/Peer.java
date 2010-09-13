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
package net.dushin.lethe.messaging.client.ui.controller;

import net.dushin.lethe.messaging.client.crypto.KeyHelper;
import net.dushin.lethe.messaging.client.crypto.Verifier;
import net.dushin.lethe.messaging.interfaces.keys.PublicKeyType;

/**
 * A Peer is a representation of an external identity,
 * encapsupated by a name and public key.
 */
public class Peer implements Comparable<Peer> {

    /**
     * the peer name
     */
    private final String name;
    
    /**
     * The public key associated with this peer
     */
    private final java.security.PublicKey publicKey;
    
    /**
     * the "pinky print" for the public key.  This
     * 8 character key is computed off the SHA1 hash
     * of the encoding of the public key.
     */
    private final String pinkyprint;

    /**
     * the Verifier used to verify messages signed by this peer.
     */
    private final Verifier verifier;
    
    private final String stringRep;
    
    /**
     * flag indicating whether messages should be encrypted
     * to this peer.
     */
    private boolean encryptTo = true;
    
    //
    // lifecycle
    //
    
    public
    Peer(
        final String name,
        final java.security.PublicKey publicKey
    ) {
        this.name = name;
        this.publicKey = publicKey;
        this.verifier = new Verifier(this.publicKey);
        this.pinkyprint = KeyHelper.getPinkyprint(this.publicKey);
        this.stringRep = KeyHelper.toString(name, publicKey);
    }
    
    public
    Peer(
        final String pkg
    ) {
        PublicKeyType pkt = KeyHelper.parse(pkg);
        this.name = pkt.getName();
        this.publicKey = KeyHelper.getPublicKey(pkt);
        this.verifier = new Verifier(this.publicKey);
        this.pinkyprint = KeyHelper.getPinkyprint(this.publicKey);
        this.stringRep = pkg;
    }
    
    //
    // public operations
    //
    
    /**
     * @return      the peer name
     */
    public String
    getName() {
        return this.name;
    }
    
    /**
     * @return      the peer's public key
     */
    public java.security.PublicKey
    getPublicKey() {
        return this.publicKey;
    }
    
    /**
     * @return      the peer's publick key pinkyprint
     */
    public String
    getPinkyprint() {
        return this.pinkyprint;
    }
    
    /**
     * @return      the peer's Verifier
     */
    public Verifier
    getVerifier() {
        return this.verifier;
    }
    
    /**
     * @return      true, if messages should be encrypted
     *              to this peer; false, otherwise
     */
    public boolean
    getEncryptTo() {
        return this.encryptTo;
    }
    
    /**
     * Indicate whether messages should be encrypted to this peer
     */
    public void
    setEncryptTo(
        final boolean encryptTo
    ) {
        this.encryptTo = encryptTo;
    }
    
    public String
    toString() {
        return this.stringRep;
    }

    public int 
    compareTo(final Peer o) {
        if (o == null) {
            throw new IllegalStateException("null compare; illegal state");
        }
        return toString().compareTo(o.toString());
    }
}
