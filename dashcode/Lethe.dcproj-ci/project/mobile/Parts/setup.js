/* 
 This file was generated by Dashcode and is covered by the 
 license.txt included in the project.  You may edit this file, 
 however it is recommended to first turn off the Dashcode 
 code generator otherwise the changes will be lost.
 */
var dashcodePartSpecs = {
    "AboutBrowser": { "clearSelectionOnBack": true, "view": "DC.Browser" },
    "AboutDetail": { "propertyValues": { "htmlBinding": { "keypath": "AboutList.selection.text" } }, "text": "Text", "view": "DC.Text" },
    "AboutList": { "allowsEmptySelection": true, "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "label1", "listStyle": "List.ROUNDED_RECTANGLE", "propertyValues": { "dataArrayBinding": { "keypath": "lethe.content.about.sections" } }, "sampleRows": 3, "selectionEnabled": true, "useDataSource": true, "view": "DC.List" },
    "AboutTitle": { "propertyValues": { "textBinding": { "keypath": "AboutList.selection.title" } }, "text": "Text", "view": "DC.Text" },
    "activityIndicator": { "propertyValues": { "animatingBinding": { "keypath": "lethe.content.starting" }, "visibleBinding": { "keypath": "lethe.content.starting" } }, "view": "DC.ActivityIndicator" },
    "button": { "initialHeight": 26, "initialWidth": 26, "leftImageWidth": 5, "onclick": "removeChannel", "propertyValues": { "enabledBinding": { "keypath": "ChannelList.hasSelection" } }, "rightImageWidth": 5, "text": "-", "view": "DC.PushButton" },
    "button1": { "initialHeight": 26, "initialWidth": 26, "leftImageWidth": 5, "onclick": "addChannel", "rightImageWidth": 5, "text": "+", "view": "DC.PushButton" },
    "button2": { "initialHeight": 30, "initialWidth": 72, "leftImageWidth": 5, "onclick": "showChannelsView", "rightImageWidth": 5, "text": "Done", "view": "DC.PushButton" },
    "button3": { "initialHeight": 30, "initialWidth": 76, "leftImageWidth": 5, "onclick": "showAboutView", "rightImageWidth": 5, "text": "About", "view": "DC.PushButton" },
    "ChannelBrowser": { "clearSelectionOnBack": true, "view": "DC.Browser" },
    "ChannelBrowserHeader": { "rootTitle": "Channels", "view": "DC.Header" },
    "ChannelBrowserStack": { "subviewsTransitions": [{ "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }], "view": "DC.StackLayout" },
    "ChannelList": { "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "label", "listStyle": "List.EDGE_TO_EDGE", "propertyValues": { "dataArrayBinding": { "keypath": "lethe.content.channels" } }, "sampleRows": 3, "selectionEnabled": true, "useDataSource": true, "view": "DC.List" },
    "ChannelStack": { "subviewsTransitions": [{ "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "flip" }, { "direction": "left-right", "duration": "", "timing": "ease-in-out", "type": "flip" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }], "view": "DC.StackLayout" },
    "DescriptionLabel": { "text": "Enter a name (optional), and generate a key.  The key will be used to sign and encrypt messages.", "view": "DC.Text" },
    "div1": { "initialHeight": 30, "initialWidth": 70, "leftImageWidth": 16, "rightImageWidth": 5, "text": "Back", "view": "DC.PushButton" },
    "div2": { "rootTitle": "About", "view": "DC.Header" },
    "div3": { "initialHeight": 30, "initialWidth": 70, "leftImageWidth": 16, "rightImageWidth": 5, "text": "Back", "view": "DC.PushButton" },
    "div4": { "subviewsTransitions": [{ "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }], "view": "DC.StackLayout" },
    "GenerateKeyIndicator": { "propertyValues": { "visibleBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "showGeneratingActivityMonitor" } }, "view": "DC.ActivityIndicator" },
    "IdentityButton": { "initialHeight": 26, "initialWidth": 56, "leftImageWidth": 5, "onclick": "modifyIdentity", "rightImageWidth": 5, "text": "Modify", "view": "DC.PushButton" },
    "IdentityCancelButton": { "initialHeight": 30, "initialWidth": 82, "leftImageWidth": 5, "onclick": "identityViewCancel", "propertyValues": { "enabledBinding": { "keypath": "lethe.content.identity.pubKey" } }, "rightImageWidth": 5, "text": "Cancel", "view": "DC.PushButton" },
    "IdentityDoneButton": { "initialHeight": 30, "initialWidth": 72, "leftImageWidth": 5, "onclick": "identityViewOk", "propertyValues": { "enabledBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "editIdentityDoneButtonEnableable" } }, "rightImageWidth": 5, "text": "Done", "view": "DC.PushButton" },
    "IdentityFingerprintDescription": { "text": "Key Fingerprint:", "view": "DC.Text" },
    "IdentityFingerprintLabel": { "propertyValues": { "textBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "IdentityFingerprintRegenerateButton": { "initialHeight": 30, "initialWidth": 95, "leftImageWidth": 5, "onclick": "generateRSAKey", "rightImageWidth": 5, "text": "Generate", "view": "DC.PushButton" },
    "IdentityKeyFingerprintLabel": { "propertyValues": { "textBinding": { "keypath": "lethe.content.identity.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "IdentityNameLabel": { "text": "Name:", "view": "DC.Text" },
    "IdentityNameTextField": { "propertyValues": { "valueBinding": { "keypath": "lethe.content.tmp.id.name" } }, "view": "DC.TextField" },
    "input": { "propertyValues": { "checkedBinding": { "keypath": "ChannelList.selection.signMessages" } }, "view": "DC.ToggleButton" },
    "input1": { "propertyValues": { "checkedBinding": { "keypath": "PeersList.selection.encryptTo" } }, "view": "DC.ToggleButton" },
    "label": { "propertyValues": { "textBinding": { "keypath": "*.name" } }, "text": "Item", "view": "DC.Text" },
    "label1": { "propertyValues": { "textBinding": { "keypath": "*.title" } }, "text": "Item", "view": "DC.Text" },
    "label2": { "propertyValues": { "textBinding": { "keypath": "*.name" } }, "text": "Item", "view": "DC.Text" },
    "LetheStack": { "subviewsTransitions": [{ "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "dissolve" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "dissolve" }, { "direction": "right-left", "duration": "", "timing": "ease-in-out", "type": "push" }], "view": "DC.StackLayout" },
    "MessagesButton": { "initialHeight": 30, "initialWidth": 101, "leftImageWidth": 5, "onclick": "showMessagesView", "rightImageWidth": 1, "text": "Messages", "view": "DC.PushButton" },
    "MessageTextArea": { "propertyValues": { "valueBinding": { "keypath": "ChannelList.selection.messages", "transformer": "messagesToText" } }, "view": "DC.TextField" },
    "PeersButton": { "initialHeight": 30, "initialWidth": 76, "leftImageWidth": 1, "onclick": "showPeersView", "rightImageWidth": 5, "text": "Peers", "view": "DC.PushButton" },
    "PeersList": { "allowsEmptySelection": true, "dataArray": ["Item 1", "Item 2", "Item 3"], "labelElementId": "label2", "listStyle": "List.ROUNDED_RECTANGLE", "propertyValues": { "dataArrayBinding": { "keypath": "ChannelList.selection.peers" } }, "sampleRows": 3, "selectionEnabled": true, "useDataSource": true, "view": "DC.List" },
    "SendMessageButton": { "initialHeight": 24, "initialWidth": 303, "leftImageWidth": 5, "onclick": "sendMessage", "rightImageWidth": 5, "text": "send", "view": "DC.PushButton" },
    "StepLabel": { "propertyValues": { "textBinding": { "keypath": "lethe.content.tmp.step" }, "visibleBinding": { "keypath": "lethe.content.tmp.id.pubKey", "transformer": "showGeneratingActivityMonitor" } }, "text": "step", "view": "DC.Text" },
    "text": { "text": "Identity", "view": "DC.Text" },
    "text1": { "propertyValues": { "textBinding": { "keypath": "PeersList.selection.name" } }, "text": "Peer Name", "view": "DC.Text" },
    "text2": { "propertyValues": { "textBinding": { "keypath": "lethe.content.identity.name" } }, "text": "Name", "view": "DC.Text" },
    "text3": { "propertyValues": { "textBinding": { "keypath": "PeersList.selection.pubKey", "transformer": "pubKeyFingerprint" } }, "text": "54 C9 1C 74 81 00 37 38 83 DD 6E 7C 03 15 4E 49 4C 94 89 9E", "view": "DC.Text" },
    "text4": { "text": "Lethe 0.1-SNASPHOT", "view": "DC.Text" },
    "text5": { "text": "Key Fingerprint:", "view": "DC.Text" },
    "text6": { "text": "Peer Name:", "view": "DC.Text" }
};

















