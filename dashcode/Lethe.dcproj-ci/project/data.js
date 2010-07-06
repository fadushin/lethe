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
    
    about: 
        "<p>Welcome to Lethe!</p>"
        + "<h2>What is Lethe?</h2>"
        + "<p>Lethe is a web-based instant messaging system, but with security built into the messaging protocol and user interface.</p>"
        + "<h2>How is Lethe different from AIM or Microsoft Messenger or IRC or ...?</h2>"
        + "<p>Lethe is a messaging system, so in some respects it's just like AOL's or Microsoft's messaging system.  You can use it to send messages to other users on the system.  What's different about Lethe is that communication beteeen participants can be cryptographically signed and encrypted.  This means that messages can be delivered to their intended recipients, and no one -- not even the NSA, can read them</p>"
        + "<h2>How does it work?</h2>"
        + "<p>When you first start Lethe, you are prompted to enter a user name.  In addition, a cryptographic key pair will be generated for you.  This key pair contains a private key and a public key.  The public key is </p>"
        /*
        + "<h2>?</h2>"
        + "<p></p>"
        */
}