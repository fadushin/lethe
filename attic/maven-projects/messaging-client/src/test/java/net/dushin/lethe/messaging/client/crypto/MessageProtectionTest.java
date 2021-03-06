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

import net.dushin.lethe.messaging.interfaces.EncryptedMessage;
import net.dushin.lethe.messaging.interfaces.PlaintextMessage;
import net.dushin.lethe.messaging.interfaces.SignedMessage;

/**
 * This test exercices the message protection operations
 * in the crypto package (encryption and signature over messages)
 */
public class MessageProtectionTest extends org.junit.Assert {
    
    //
    // test data
    //
    
    private static final String ALICE_PW = "alice";
    /*
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
    */

    private static final String BOB_PW = "bob";
    /*
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
    */

    private static final PlaintextMessage ALICE_MSG = new PlaintextMessage();
    static {
        ALICE_MSG.setFrom("alice");
        ALICE_MSG.setData("'Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe;");
    }

    private static final PlaintextMessage BOB_MSG = new PlaintextMessage();
    static {
        BOB_MSG.setFrom("bob");
        BOB_MSG.setData("All mimsy were the borogoves,\nAnd the mome raths outgrabe.");
    }
    

    /**
     * test plaintext encryption
     */
    @org.junit.Test
    public final void
    testPlaintextEncryption() throws Exception {
        try {
            testPlaintextEncryption(512, 1024);
            testPlaintextEncryption(1024, 512);
            //
            // commenting out for now; these are slooow tests
            //
            // testPlaintextEncryption(2048, 512);
            // testPlaintextEncryption(4096, 512);
        } catch (final Exception e) {
            e.printStackTrace();
            fail("testEncryption failed for the above reason");
        }
    }
    
    private void
    testPlaintextEncryption(
        final int expectedKeySize,
        final int unexpectedKeySize
    ) throws Exception {
        try {
            final java.security.KeyPair alice =
                new KeyPairGenerator(expectedKeySize).generateKeyPair(ALICE_PW);
            final java.security.KeyPair bob =
                new KeyPairGenerator(expectedKeySize).generateKeyPair(BOB_PW);
            //
            // success scenarios
            //
            testPlaintextEncryption(
                alice.getPublic(),
                alice.getPrivate(),
                ALICE_MSG,
                true
            );
            testPlaintextEncryption(
                alice.getPublic(),
                new KeyPairGenerator(expectedKeySize).generateKeyPair(ALICE_PW).getPrivate(),
                ALICE_MSG,
                true
            );
            testPlaintextEncryption(
                new KeyPairGenerator(expectedKeySize).generateKeyPair(ALICE_PW).getPublic(),
                alice.getPrivate(),
                ALICE_MSG,
                true
            );
            //
            // failure scenarios
            //
            testPlaintextEncryption(
                new KeyPairGenerator(unexpectedKeySize).generateKeyPair(ALICE_PW).getPublic(),
                alice.getPrivate(),
                ALICE_MSG,
                false
            );
            testPlaintextEncryption(
                alice.getPublic(),
                bob.getPrivate(),
                ALICE_MSG,
                false
            );
            testPlaintextEncryption(
                bob.getPublic(),
                alice.getPrivate(),
                ALICE_MSG,
                false
            );
        } catch (final Exception e) {
            e.printStackTrace();
            fail("testEncryption failed for the above reason");
        }
    }

    private static void
    testPlaintextEncryption(
        final java.security.PublicKey encryptionKey,
        final java.security.PrivateKey decryptionKey,
        final PlaintextMessage message,
        final boolean expectSuccess
    ) {
        try {
            final Encryptor encryptor = new Encryptor();
            final java.util.List<java.security.PublicKey> keys =
                new java.util.ArrayList<java.security.PublicKey>();
            keys.add(encryptionKey);
            //
            // encrypt the message
            //
            EncryptedMessage msg = encryptor.encrypt(message, keys);
            assertNotNull(msg);
            //
            // decrypt the message and check the results
            //
            Decryptor decryptor = new Decryptor(
                decryptionKey
            );
            Object obj = decryptor.decrypt(msg);
            assertNotNull(obj);
            assertTrue(messageEquals(message, obj));
            if (!expectSuccess) {
                fail("Expected failure");
            }
        } catch (final Exception e) {
            if (expectSuccess) {
                e.printStackTrace();
                fail("testEncryption failed for the above reason");
            }
        }
    }

    /**
     * test plaintext signature
     */
    @org.junit.Test
    public final void
    testPlaintextSignature() throws Exception {
        try {
            testPlaintextSignature(512, 1024);
            testPlaintextSignature(1024, 512);
            //
            // commenting out for now; these are slooow tests
            //
            // testPlaintextSignature(2048, 512);
            // testPlaintextSignature(4096, 512);
        } catch (final Exception e) {
            e.printStackTrace();
            fail("testPlaintextSignature failed for the above reason");
        }
    }
    
    private void
    testPlaintextSignature(
        final int expectedKeySize,
        final int unexpectedKeySize
    ) throws Exception {
        try {
            final java.security.KeyPair alice =
                new KeyPairGenerator(expectedKeySize).generateKeyPair(ALICE_PW);
            final java.security.KeyPair bob =
                new KeyPairGenerator(expectedKeySize).generateKeyPair(BOB_PW);
            //
            // success scenarios
            //
            testPlaintextSignature(
                alice.getPrivate(),
                alice.getPublic(),
                ALICE_MSG,
                true
            );
            testPlaintextSignature(
                new KeyPairGenerator(expectedKeySize).generateKeyPair(ALICE_PW).getPrivate(),
                alice.getPublic(),
                ALICE_MSG,
                true
            );
            testPlaintextSignature(
                alice.getPrivate(),
                new KeyPairGenerator(expectedKeySize).generateKeyPair(ALICE_PW).getPublic(),
                ALICE_MSG,
                true
            );
            //
            // failure scenarios
            //
            testPlaintextSignature(
                alice.getPrivate(),
                new KeyPairGenerator(unexpectedKeySize).generateKeyPair(ALICE_PW).getPublic(),
                ALICE_MSG,
                false
            );
            testPlaintextSignature(
                alice.getPrivate(),
                bob.getPublic(),
                ALICE_MSG,
                false
            );
            testPlaintextSignature(
                bob.getPrivate(),
                alice.getPublic(),
                ALICE_MSG,
                false
            );
        } catch (final Exception e) {
            e.printStackTrace();
            fail("testPlaintextSignature failed for the above reason");
        }
    }
    
    private static void
    testPlaintextSignature(
        final java.security.PrivateKey signingKey,
        final java.security.PublicKey verificationKey,
        final PlaintextMessage message,
        final boolean expectSuccess
    ) {
        try {
            final Signer signer = new Signer(signingKey);
            //
            // sign the message
            //
            final SignedMessage signed = signer.sign(message);
            assertNotNull(signed);
            //
            // Check that it verifies
            //
            final Verifier verifier = new Verifier(verificationKey);
            final Object obj = verifier.verify(signed);
            assertNotNull(obj);
            assertTrue(messageEquals(message, obj));
            //
            // Check that tampering fails
            //
            final byte[] bra = signed.getSerializedMessage();
            bra[0] = (byte) (bra[0] ^ 0xFF);
            try {
                verifier.verify(signed);
                fail("expected failure on tampering");
            } catch (final Exception e) {
                // ok
            }
            if (!expectSuccess) {
                fail("Expected failure");
            }
        } catch (final Exception e) {
            if (expectSuccess) {
                e.printStackTrace();
                fail("testEncryption failed for the above reason");
            }
        }
    }
    @org.junit.Test
    public final void
    testSignedEncryption() throws Exception {
        try {
            testSignedEncryption(512, 1024);
            testSignedEncryption(1024, 512);
            //
            // commenting out for now; these are slooow tests
            //
            // testSignedEncryption(2048, 512);
            // testSignedEncryption(4096, 512);
        } catch (final Exception e) {
            e.printStackTrace();
            fail("testSignedEncryption failed for the above reason");
        }
    }
    
    private void
    testSignedEncryption(
        final int expectedKeySize,
        final int unexpectedKeySize
    ) throws Exception {
        try {
            final java.security.KeyPair alice =
                new KeyPairGenerator(expectedKeySize).generateKeyPair(ALICE_PW);
            final java.security.KeyPair bob =
                new KeyPairGenerator(expectedKeySize).generateKeyPair(BOB_PW);
            //
            // success scenarios
            //
            testSignedEncryption(
                alice.getPrivate(),
                bob.getPublic(),
                bob.getPrivate(),
                alice.getPublic(),
                ALICE_MSG,
                true
            );
        } catch (final Exception e) {
            e.printStackTrace();
            fail("testSignedEncryption failed for the above reason");
        }
    }

    private static void
    testSignedEncryption(
        final java.security.PrivateKey signingKey,
        final java.security.PublicKey encryptionKey,
        final java.security.PrivateKey decryptionKey,
        final java.security.PublicKey verificationKey,
        final PlaintextMessage message,
        final boolean expectSuccess
    ) {
        try {
            //
            // sign the message
            //
            final Signer signer = new Signer(signingKey);
            final SignedMessage signed = signer.sign(message);
            assertNotNull(signed);
            //
            // encrypt the signed message
            //
            final Encryptor encryptor = new Encryptor();
            final java.util.List<java.security.PublicKey> keys =
                new java.util.ArrayList<java.security.PublicKey>();
            keys.add(encryptionKey);
            EncryptedMessage encrypted = encryptor.encrypt(signed, keys);
            assertNotNull(encrypted);
            //
            // decrypt the signed message and check the results
            //
            Decryptor decryptor = new Decryptor(
                decryptionKey
            );
            Object decrypted = decryptor.decrypt(encrypted);
            assertTrue(decrypted instanceof SignedMessage);
            SignedMessage decryptedSigned = (SignedMessage) decrypted;
            //
            // Check that it verifies
            //
            final Verifier verifier = new Verifier(verificationKey);
            final Object obj = verifier.verify(decryptedSigned);
            assertNotNull(obj);
            assertTrue(messageEquals(message, obj));
            if (!expectSuccess) {
                fail("Expected failure");
            }
        } catch (final Exception e) {
            if (expectSuccess) {
                e.printStackTrace();
                fail("testEncryption failed for the above reason");
            }
        }
    }
    
    private static boolean
    messageEquals(
        final PlaintextMessage msg,
        final Object obj
    ) {
        if (!(obj instanceof PlaintextMessage)) {
            return false;
        }
        final PlaintextMessage m2 = (PlaintextMessage) obj;
        return msg.getFrom().equals(m2.getFrom()) 
            && msg.getData().equals(m2.getData());
    }
}
