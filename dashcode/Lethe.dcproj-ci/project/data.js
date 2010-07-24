{
    identity: {
        name: "",
        privKey: "",
        pubKey: ""
    },
    channels: [
        /*
        { 
            name: "Acadia",
            signMessages: true,
            peers: [
                {
                    name: "Margot",
                    pubKey: "gRhttTO5x1Cr5AJ3Aw1bOv1Nq8aTkkz2lqIeku2d7X4y/sMQ/gzv1vp7repv/20i0tPGgR8rQud4uDuBz32ANCjGdViXtXOdWJo2kE1HnlOJ++ZSTFXcCxCUNhnDb1OPxotfKXRxNw5P/PYP1UfmKQ==",
                    encryptTo: true 
                },
                {
                    name: "Fred",
                    pubKey: "1zTbIQhQxlWPbvSb1G9TWe/Wani/Y2kqRUOGswEPLq3kEmgOLicwQ4lHFUvuBDydDrZcThyU/eoRsJ2sN0hYkI/wnfnu8IONIf8eLNmeI8KnYSX3jY8QA1L7rgMmGfXAVcpYUlNQXmLjq3sqlWbe8Q==",
                    encryptTo: false 
                }
            ],
            messages: [
                {
                    from: "Fred",
                    message: "Hi"
                },
                {
                    from: "Margot",
                    message: "Hi there"
                }
            ]
        }
        */
    ],
    
    tmp: {
        id: {
            name: "",
            privKey: "",
            pubKey: ""
        }
    },
    
    about: {
        sections: [
            {
                title: "Welcome to Lethe",
                text: "Welcome to Lethe, a web-based messaging system, but with security built into the messaging protocol and user interface."
            },
            {
                title: "What makes Lethe different?",
                text: "Lethe is a messaging system, so in some respects it's just like AOL's or Microsoft's messaging systems.  You can use it to send messages to other users on the system.  What's different about Lethe is that communication between participants can be cryptographically signed and encrypted.  This means that messages can be delivered to their intended recipients, and no one -- not even the NSA, can read them.  Well, okay, maybe the NSA could if they tried.  They have big guns.  But the casual interloper cannot."
            },
            {
                title: "How does it work?",
                text: "When you first start Lethe, a cryptographic key pair is automatically generated for you.  The private key from the key pair is kept locally in the browser.  The public key is published to the Lethe server, when you join a channel.  Any peer on the same channel receives your public key when they join the channel.  You can use a peer's public key to encrypt messages to that peer.  You can also designate other peers, including yourself, as recipients.  Only the specified recipients can decrypt the messages, for only those peers posess the private key needed to decrypt the message.  You can also use your private key to sign messages.  When a peer receives a signed message, the signature is validated against the public keys in the list of peers on the channel.  When a message is signed and validated in this way, message recipients are guaranteed that the message has not been modified in transit, for only the sender posesses the private key used to sign the message."
            },
            {
                title: "Couldn't you just do this with SSL?",
                text: "Sure, if you trust your messanging middleware.  SSL just protects you from point-to-point, like from your browser to the web server.  Once the message makes it to the server, it's basically in plaintext, for anyone to see.  Do you trust the web admins to not snoop on your messages?  Maybe you do.  I know people who used to work on the graveyard shift as telephone operators.  (Remember them?)  When they were bored in the middle of the night, they'd listen in on phone conversations.  \"It Can't Happen Here.\""
            },
            {
                title: "What about Trust?",
                text: "Lethe doesn't provide any mechanism for trust establishment.  If Alice and Bob are communicating over a Lethe channel, there is no way in Lethe for Bob to know that he has Alice\'s public key, and conversely.  The only way for Bob and Alice to establish trust in each other's keys is to share keys (or key fingerprints, which are the next best thing) out of band, such as through email, the telephone, or a dark ally."
            },
            {
                title: "I want to know more about the Crypto",
                text: "It's all there in the code.  Crack open the debugger.  (WebKit rocks, by the way).  This is a POC.  Read the implementation.  Future versions will go into excruciating detail about this, and will probably allow you to modify behaviors."
            },
            {
                title: "Is this a Beta?",
                text: "It's barely an alpha.  There are lots of known issues.  I'm just putting Lethe up on a server to get some help debugging.  Help.  I have a day job -- just barely.  And a family who would like more of my time.  Etc.  I'm just trying to get this into a state where it works enough."
            },
            {
                title: "How is Lethe licensed?",
                text: "Lethe is licensed under the terms of the BSD License.  However, Lethe is built on top of core cryptographic and platform libraries that are distributed under the terms of the Lesser Gnu Public License (LGPL).  Portions of Lethe are copyrighted by Atsushi Oka (http://ats.oka.nu/).  Portions of Lethe are copyrighted by Tom Wu (http://www-cs-students.stanford.edu/~tjw/jsbn/).  Portions of Lethe are copyrighted by Robert Kieffer (http://www.broofa.com).  Portions of Lethe are copyrighted by JSOlait (http://jsolait.net/).  Suffice it to say we are standing on the shoulders of giants."
            },
            {
                title: "Lethe License",
                text: "Copyright (c) dushin.net\nAll rights reserved.\n\nRedistribution and use in source and binary forms, with or without\nmodification, are permitted provided that the following conditions are met:\n    * Redistributions of source code must retain the above copyright\n      notice, this list of conditions and the following disclaimer.\n    * Redistributions in binary form must reproduce the above copyright\n      notice, this list of conditions and the following disclaimer in the\n      documentation and/or other materials provided with the distribution.\n    * Neither the name of dushin.net nor the\n      names of its contributors may be used to endorse or promote products\n       derived from this software without specific prior written permission.\n\nTHIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY\nEXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\nWARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\nDISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY\nDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\nLOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND\nON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\nSOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
            }
        ]
    }
}