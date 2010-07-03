/* 
 This file was generated by Dashcode and is covered by the 
 license.txt included in the project.  You may edit this file, 
 however it is recommended to first turn off the Dashcode 
 code generator otherwise the changes will be lost.
 */
var dashcodePartSpecs = {
    "AddChannelButton": { "initialHeight": 20, "initialWidth": 20, "leftImageWidth": 5, "onclick": "addChannel", "rightImageWidth": 5, "text": "+", "view": "DC.PushButton" },
    "button": { "initialHeight": 30, "initialWidth": 56, "leftImageWidth": 5, "onclick": "editIdentityViewOk", "propertyValues": { "enabledBinding": { "keypath": "lethe.content.tmp.id.pubKey" } }, "rightImageWidth": 5, "text": "ok", "view": "DC.PushButton" },
    "button1": { "initialHeight": 30, "initialWidth": 80, "leftImageWidth": 5, "onclick": "editIdentityViewCancel", "rightImageWidth": 5, "text": "cancel", "view": "DC.PushButton" },
    "ChannelLabel": { "text": "Channels", "view": "DC.Text" },
    "ChannelList": { "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "rowLabel", "listStyle": "List.DESKTOP_LIST", "propertyValues": { "dataArrayBinding": { "keypath": "lethe.content.channels" } }, "sampleRows": 3, "selectionEnabled": true, "useDataSource": true, "view": "DC.List" },
    "ChannelStackLayout": { "propertyValues": { "visibleBinding": { "keypath": "ChannelList.hasSelection" } }, "subviewsTransitions": [{ "direction": "left-right", "duration": "", "timing": "ease-in-out", "type": "push" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }], "view": "DC.StackLayout" },
    "EncryptToLabel": { "text": "Encrypt", "view": "DC.Text" },
    "GenerateKeyButton1": { "initialHeight": 23, "initialWidth": 86, "leftImageWidth": 5, "onclick": "generateRSAKey", "rightImageWidth": 5, "text": "Regenerate", "view": "DC.PushButton" },
    "GenerateKeyIndicator": { "propertyValues": { "animatingBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "mappubKeyToAnimating" }, "visibleBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "mappubKeyToAnimating" } }, "view": "DC.ActivityIndicator" },
    "IdentityNameLabel": { "text": "Name:", "view": "DC.Text" },
    "IdentityNameTextLabel": { "propertyValues": { "textBinding": { "keypath": "lethe.content.identity.name" } }, "text": "Text", "view": "DC.Text" },
    "input": { "propertyValues": { "checkedBinding": { "keypath": "*.encryptTo" } }, "view": "DC.ToggleButton" },
    "input1": { "propertyValues": { "checkedBinding": { "keypath": "ChannelList.selection.signMessages" } }, "view": "DC.ToggleButton" },
    "KeyFingerprint": { "propertyValues": { "textBinding": { "keypath": "lethe.content.identity.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "KeyFingerprint1": { "propertyValues": { "textBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "KeyFingerprintLabel": { "text": "Key Fingerprint:", "view": "DC.Text" },
    "MainStackLayout": { "subviewsTransitions": [{ "direction": "left-right", "duration": "", "timing": "ease-in-out", "type": "none" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "none" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }], "view": "DC.StackLayout" },
    "MessagesLabel": { "text": "Messages", "view": "DC.Text" },
    "MessagesTextArea": { "propertyValues": { "valueBinding": { "keypath": "ChannelList.selection.messages", "transformer": "messagesToText" } }, "view": "DC.TextField" },
    "ModifyIdentityNameButton": { "initialHeight": 23, "initialWidth": 61, "leftImageWidth": 5, "onclick": "modifyIdentity", "rightImageWidth": 5, "text": "Modify...", "view": "DC.PushButton" },
    "PeerFingerprintHeaderLabel": { "text": "Key Fingerprint", "view": "DC.Text" },
    "PeerFingerprintLabel": { "propertyValues": { "textBinding": { "keypath": "*.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "PeerNameHeaderLabel": { "text": "Peer Name", "view": "DC.Text" },
    "PeerNameLabel": { "propertyValues": { "textBinding": { "keypath": "*.name" } }, "text": "name", "view": "DC.Text" },
    "PeersLabel": { "text": "Peers", "view": "DC.Text" },
    "PeersList": { "allowsEmptySelection": true, "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "PeerNameLabel", "listStyle": "List.DESKTOP_LIST", "propertyValues": { "dataArrayBinding": { "keypath": "ChannelList.selection.peers" } }, "sampleRows": 3, "useDataSource": true, "view": "DC.List" },
    "RemoveChannelButton": { "initialHeight": 20, "initialWidth": 20, "leftImageWidth": 5, "onclick": "removeChannel", "propertyValues": { "enabledBinding": { "keypath": "ChannelList.hasSelection" } }, "rightImageWidth": 5, "text": "-", "view": "DC.PushButton" },
    "rowLabel": { "propertyValues": { "textBinding": { "keypath": "*.name" } }, "text": "Item", "view": "DC.Text" },
    "SendMessageButton": { "initialHeight": 21, "initialWidth": 71, "leftImageWidth": 5, "onclick": "sendMessage", "rightImageWidth": 5, "text": "Send", "view": "DC.PushButton" },
    "splitLayout": { "flexibleViewIndex": 1, "initialSize": 876, "initialSplitterSize": 8, "isVertical": true, "splitterPosition": 159, "view": "DC.SplitLayout" },
    "SwitchToMessagesViewButton": { "initialHeight": 22, "initialWidth": 95, "leftImageWidth": 5, "onclick": "switchToMessagesView", "rightImageWidth": 5, "text": "Messages...", "view": "DC.PushButton" },
    "SwitchToPeersViewButton": { "initialHeight": 21, "initialWidth": 64, "leftImageWidth": 5, "onclick": "switchToPeersView", "rightImageWidth": 5, "text": "Peers...", "view": "DC.PushButton" },
    "text": { "text": "Name:", "view": "DC.Text" },
    "text3": { "text": "Key Fingerprint:", "view": "DC.Text" },
    "textField": { "propertyValues": { "valueBinding": { "keypath": "lethe.content.tmp.id.name" } }, "view": "DC.TextField" },
    "TitleText": { "text": "Lethe Messenger", "view": "DC.Text" },
    "ViewPeersButton1": { "initialHeight": 24, "initialWidth": 77, "leftImageWidth": 12, "rightImageWidth": 12, "text": "Peers...", "view": "DC.PushButton" }
};



























