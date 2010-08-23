/* 
 This file was generated by Dashcode and is covered by the 
 license.txt included in the project.  You may edit this file, 
 however it is recommended to first turn off the Dashcode 
 code generator otherwise the changes will be lost.
 */
var dashcodePartSpecs = {
    "AboutBoxOkButton": { "initialHeight": 30, "initialWidth": 56, "leftImageWidth": 5, "onclick": "showChannelsView", "rightImageWidth": 5, "text": "ok", "view": "DC.PushButton" },
    "AboutBoxSplitLayout": { "flexibleViewIndex": 1, "initialSize": 784, "initialSplitterSize": 10, "isVertical": true, "splitterPosition": 192, "view": "DC.SplitLayout" },
    "AboutButton": { "initialHeight": 21, "initialWidth": 64, "leftImageWidth": 5, "onclick": "showAboutView", "rightImageWidth": 5, "text": "About...", "view": "DC.PushButton" },
    "AboutDetailTitle": { "propertyValues": { "textBinding": { "keypath": "AboutList.selection.title" } }, "text": "Text", "view": "DC.Text" },
    "AboutDetailView": { "propertyValues": { "visibleBinding": { "keypath": "AboutList.hasSelection" } }, "view": "DC.View" },
    "AboutList": { "allowsEmptySelection": true, "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "rowLabel1", "listStyle": "List.DESKTOP_LIST", "propertyValues": { "dataArrayBinding": { "keypath": "lethe.content.about.sections" } }, "sampleRows": 3, "selectionEnabled": true, "useDataSource": true, "view": "DC.List" },
    "activityIndicator": { "propertyValues": { "animatingBinding": { "keypath": "lethe.content.starting" }, "visibleBinding": { "keypath": "lethe.content.starting" } }, "view": "DC.ActivityIndicator" },
    "AddChannelButton": { "initialHeight": 20, "initialWidth": 20, "leftImageWidth": 5, "onclick": "addChannel", "rightImageWidth": 5, "text": "+", "view": "DC.PushButton" },
    "box3": { "propertyValues": { "htmlBinding": { "keypath": "AboutList.selection.text" } }, "view": "DC.View" },
    "ChannelBox": { "propertyValues": { "visibleBinding": { "keypath": "ChannelList.hasSelection" } }, "view": "DC.View" },
    "ChannelLabel": { "text": "Channels", "view": "DC.Text" },
    "ChannelList": { "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "rowLabel", "listStyle": "List.DESKTOP_LIST", "propertyValues": { "dataArrayBinding": { "keypath": "lethe.content.channels" } }, "sampleRows": 3, "selectionEnabled": true, "useDataSource": true, "view": "DC.List" },
    "ChannelSplitLayout": { "flexibleViewIndex": 1, "initialSize": 959, "initialSplitterSize": 8, "isVertical": true, "splitterPosition": 178, "view": "DC.SplitLayout" },
    "ChannelStack": { "subviewsTransitions": [{ "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "flip" }, { "direction": "left-right", "duration": "", "timing": "ease-in-out", "type": "flip" }], "view": "DC.StackLayout" },
    "DescriptionLabel": { "text": "Enter a name (optional), and generate a key.  The key will be used to sign and encrypt messages.", "view": "DC.Text" },
    "EditIdentityCancelButton": { "initialHeight": 30, "initialWidth": 80, "leftImageWidth": 5, "onclick": "identityViewCancel", "propertyValues": { "enabledBinding": { "keypath": "lethe.content.identity.pubKey" } }, "rightImageWidth": 5, "text": "cancel", "view": "DC.PushButton" },
    "EditIdentityOkButton": { "initialHeight": 30, "initialWidth": 56, "leftImageWidth": 5, "onclick": "identityViewOk", "propertyValues": { "enabledBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "editIdentityDoneButtonEnableable" } }, "rightImageWidth": 5, "text": "ok", "view": "DC.PushButton" },
    "EncryptToLabel": { "text": "Encrypt", "view": "DC.Text" },
    "GenerateKeyButton1": { "initialHeight": 23, "initialWidth": 72, "leftImageWidth": 5, "onclick": "generateRSAKey", "rightImageWidth": 5, "text": "Generate", "view": "DC.PushButton" },
    "GenerateKeyIndicator": { "propertyValues": { "animatingBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "showGeneratingActivityMonitor" }, "visibleBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "showGeneratingActivityMonitor" } }, "view": "DC.ActivityIndicator" },
    "IdentityNameLabel": { "text": "Name:", "view": "DC.Text" },
    "IdentityNameTextLabel": { "propertyValues": { "textBinding": { "keypath": "lethe.content.identity.name" } }, "text": "Text", "view": "DC.Text" },
    "image1": { "view": "DC.ImageLayout" },
    "input": { "propertyValues": { "checkedBinding": { "keypath": "*.encryptTo" } }, "view": "DC.ToggleButton" },
    "input1": { "propertyValues": { "checkedBinding": { "keypath": "ChannelList.selection.signMessages" } }, "view": "DC.ToggleButton" },
    "input2": { "propertyValues": { "checkedBinding": { "keypath": "PeersList.selection.isTrusted" } }, "view": "DC.ToggleButton" },
    "KeyFingerprint": { "propertyValues": { "textBinding": { "keypath": "lethe.content.identity.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "KeyFingerprint1": { "propertyValues": { "textBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "KeyFingerprintLabel": { "text": "Key Fingerprint:", "view": "DC.Text" },
    "LetheStack": { "subviewsTransitions": [{ "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "none" }, { "direction": "left-right", "duration": "", "timing": "ease-in-out", "type": "none" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "none" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }], "view": "DC.StackLayout" },
    "MessagesButton": { "initialHeight": 30, "initialWidth": 101, "leftImageWidth": 5, "onclick": "showMessagesView", "rightImageWidth": 1, "text": "Messages", "view": "DC.PushButton" },
    "MessageTextBox": { "propertyValues": { "htmlBinding": { "keypath": "ChannelList.selection.messages", "transformer": "messagesToHtml" } }, "view": "DC.View" },
    "ModifyIdentityNameButton": { "initialHeight": 23, "initialWidth": 61, "leftImageWidth": 5, "onclick": "modifyIdentity", "rightImageWidth": 5, "text": "Modify...", "view": "DC.PushButton" },
    "ModifyIdentityNameButton1": { "initialHeight": 23, "initialWidth": 95, "leftImageWidth": 5, "onclick": "showTrustedPeersView", "rightImageWidth": 5, "text": "Trusted Peers", "view": "DC.PushButton" },
    "PeerFingerprintHeaderLabel": { "text": "Key Fingerprint", "view": "DC.Text" },
    "PeerFingerprintHeaderLabel1": { "text": "Key Fingerprint", "view": "DC.Text" },
    "PeerFingerprintHeaderLabel2": { "text": "Trust", "view": "DC.Text" },
    "PeerFingerprintLabel": { "propertyValues": { "textBinding": { "keypath": "*.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "PeerFingerprintLabel1": { "propertyValues": { "textBinding": { "keypath": "*.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "PeerNameHeaderLabel": { "text": "Peer Name", "view": "DC.Text" },
    "PeerNameHeaderLabel1": { "text": "Peer Name", "view": "DC.Text" },
    "PeerNameLabel": { "propertyValues": { "textBinding": { "keypath": "*.name" } }, "text": "name", "view": "DC.Text" },
    "PeerNameLabel1": { "propertyValues": { "textBinding": { "keypath": "*.name" } }, "text": "name", "view": "DC.Text" },
    "PeersButton": { "initialHeight": 30, "initialWidth": 76, "leftImageWidth": 1, "onclick": "showPeersView", "rightImageWidth": 5, "text": "Peers", "view": "DC.PushButton" },
    "PeersList": { "allowsEmptySelection": true, "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "PeerNameLabel", "listStyle": "List.DESKTOP_LIST", "propertyValues": { "dataArrayBinding": { "keypath": "ChannelList.selection.peers" } }, "sampleRows": 3, "useDataSource": true, "view": "DC.List" },
    "RemoveChannelButton": { "initialHeight": 20, "initialWidth": 20, "leftImageWidth": 5, "onclick": "removeChannel", "propertyValues": { "enabledBinding": { "keypath": "ChannelList.hasSelection" } }, "rightImageWidth": 5, "text": "-", "view": "DC.PushButton" },
    "rowLabel": { "propertyValues": { "textBinding": { "keypath": "*.name" } }, "text": "Item", "view": "DC.Text" },
    "rowLabel1": { "propertyValues": { "textBinding": { "keypath": "*.title" } }, "text": "Item", "view": "DC.Text" },
    "SendMessageButton": { "initialHeight": 21, "initialWidth": 71, "leftImageWidth": 5, "onclick": "sendMessage", "rightImageWidth": 5, "text": "Send", "view": "DC.PushButton" },
    "StepLabel": { "propertyValues": { "textBinding": { "keypath": "lethe.content.tmp.step" }, "visibleBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "showGeneratingActivityMonitor" } }, "text": "step", "view": "DC.Text" },
    "text": { "text": "Name:", "view": "DC.Text" },
    "text1": { "text": "Trusted Peers", "view": "DC.Text" },
    "text2": { "text": "Built with Dashcode", "view": "DC.Text" },
    "text3": { "text": "Key Fingerprint:", "view": "DC.Text" },
    "text5": { "text": "Lethe 0.1-SNASPHOT", "view": "DC.Text" },
    "textField": { "propertyValues": { "valueBinding": { "keypath": "lethe.content.tmp.id.name" } }, "view": "DC.TextField" },
    "TitleText": { "text": "Lethe Messenger 0.1-SNAPSHOT", "view": "DC.Text" },
    "TrustedPeersBoxOkButton": { "initialHeight": 30, "initialWidth": 56, "leftImageWidth": 5, "onclick": "showChannelsView", "rightImageWidth": 5, "text": "ok", "view": "DC.PushButton" },
    "TrustedPeersList": { "allowsEmptySelection": true, "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "PeerNameLabel1", "listStyle": "List.DESKTOP_LIST", "propertyValues": { "dataArrayBinding": { "keypath": "lethe.content.trustedPeers" } }, "sampleRows": 3, "useDataSource": true, "view": "DC.List" },
    "ViewPeersButton1": { "initialHeight": 24, "initialWidth": 77, "leftImageWidth": 12, "rightImageWidth": 12, "text": "Peers...", "view": "DC.PushButton" }
};























































