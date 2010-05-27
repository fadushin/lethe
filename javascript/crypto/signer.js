



var Signer = {};


Signer.sign = function(object, serialize, hash, sign, key) {
    
    var serializedObject = serialize(object);s
    var hashValue = hash(serializedObject);
    return {
        type: "signed",
        serialized: serializedObject,
        hash: hashValue,
        signature: sign(key, hashValue)
    };
}


