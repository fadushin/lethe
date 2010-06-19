{
    identity: {
        name: "Fred",
        privateKey: "dskfjhskjdhfkjshdfkjhsdjkf",
        publicKey: "weio234djhfkjsdhfkshdfkjhsdkjf==",
        signMessages: true
    },
    channels: [
        { 
            name: "Acadia",
            peers: [
                {
                    name: "Margot",
                    publicKey: "dskfjskldjfksdjf",
                    encryptTo: true 
                },
                {
                    name: "Fred",
                    publicKey: "sdhajksdjlkjfdksjfs==",
                    encryptTo: true 
                }
            ],
            messages: [
                {
                    message: "Hi"
                },
                {
                    message: "Hi there"
                }
            ]
        }
    ]
}