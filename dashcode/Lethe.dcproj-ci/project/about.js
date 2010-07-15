{
    sections: [
        {
            title: "Welcome to Lethe",
            text: ""
        },
        {
            title: "What is Lethe?",
            text: "Lethe is a web-based instant messaging system, but with public key cryptography built into the messaging protocol and user interface."
        },
        {
            title: "What makes Lethe different?",
            text: "Lethe is a messaging system, so in some respects it's just like AOL's or Microsoft's messaging systems.  You can use it to send messages to other users on the system.  What's different about Lethe is that communication between participants can be cryptographically signed and encrypted.  This means that messages can be delivered to their intended recipients, and no one -- not even the NSA, can read them."
        },
        {
            title: "How does it work?",
            text: "When you first start Lethe, a cryptographic key pair is automatically generated for you.  The private key from the key pair is kept locally in the browser.  The public key is published to the Lethe server, when you join a channel.  Any peer on the same channel receives your public key when they join the channel.  You can use a peer's public key to encrypt messages to that peer.  You can also designate other peers, including yourself, as recipients.  Only the specified recipients can decrypt the messages, for only those peers posess the private key needed to decrypt the message.  You can also use your private key to sign messages.  When a peer receives a signed message, the signature is validated against the public keys in the list of peers on the channel.  When a message is signed and validated in this way, message recipients are guaranteed that the message has not been modified in transit, for only the sender posesses the private key used to sign the message."
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
            title: "How is Lethe licensed?",
            text: "Lethe is licensed under the terms of the BSD License.  However, Lethe is built on top of core cryptographic and platform libraries that are distributed under the terms of the Lesser Gnu Public License (LGPL).  Portions of Lethe are copyright Atsushi Oka (http://ats.oka.nu/).  Portions of Lethe are copyright Tom Wu (http://www-cs-students.stanford.edu/~tjw/jsbn/).  Portions of Lethe are copyright Robert Kieffer (http://www.broofa.com).  Portions of Lethe are copyright JSOlait (http://jsolait.net/).  If and when Lethe is ever released, this section will provide more detailed information.  Suffice it to say we are standing on the shoulders of giants."
        },
        {
            title: "Is this a Beta?",
            text: "It's barely an alpha.  There are lots of known issues.  I'm just putting Lethe up on a server to get some help debugging.  Help.  I have a day job -- just barely.  And a family.  Etc.  I'm just trying to get this into a state where it works enough."
        }
    ]
}