

var Encryptor = {};


Encryptor.encrypt = function(object, recipientList, cipher, serialize) {

    var serializedObject = serialize(object);
    var data = cipher.encrypt(serializedObject);
    
    return {
        type: "encrypted",
        recipients: encryptKeyForRecipients(cipher.key, recipientList),
        cipherName: cipher.name,
        cipherText: data
    };
}
