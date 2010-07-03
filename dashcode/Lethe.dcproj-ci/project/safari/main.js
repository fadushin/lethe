//
// Copyright (c) dushin.net
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of dushin.net nor the
//       names of its contributors may be used to endorse or promote products
//       derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

/* 
 This file was generated by Dashcode.  
 You may edit this file to customize your widget or web page 
 according to the license.txt file included in the project.
 */


//
// Function: load()
// Called by HTML body element's onload event when the web application is ready to start
//
function load()
{
    dashcode.setupParts();
    
    setTimeout(
        function() {
            var lethe = Lethe.init();
            var identity = lethe.getStoredIdentity();
            if (identity) {
                lethe.getIdentity().assign(identity);
            } else {
                modifyIdentity();
            }
        }, 
        1000
    );
}

function generateRSAKey(event)
{
    var identity = Lethe.instance.getTmpIdentity();
    identity.setValueForKeyPath(
        'generating...',
        "pubKey"
    );
    setTimeout(
        function() {
            
            var rsa = net_dushin_crypto.KeyUtil.createRSA();
            rsa.generate(512, 65537);
            var privKey = net_dushin_crypto.KeyUtil.encodePrivateKey(rsa);
            var pubKey = net_dushin_crypto.KeyUtil.encodePublicKey(rsa);
            /*
            var privKey = "aoFxsuIseV60gPuu0+A5ZquiEHVqSBNYab/9ZXJ/Ori4DbP89glWu64tMO8KiF80jcqq1n4HRPSiqQ98zu+yYU+ZCe22+nmLsiLgp+Iq9+nnhmKyHcVlJPpYC4OpjdCmPA7J+/ZaTbwVKijlq0vhnwF02eDwqENTxC0l/9yJD1NrmfBB9quyQTTSaugjfkinUxo4THwnaRlg7+7UhwePEnUmjWaWM2L0tymfX+NRJwQ=";
            var pubKey = "/1WTr0cySIZs6Y8d5nLrywjAaCLbcSKmpESifeDUvGTiLx6v0/POtb/zbGgA1z9nhnN6EtfADwOnbW3eDbx8NhjtH9EZeH0q3oc5Ks+q3yjYJPATkndVV5zokfr8S5SsHg9Da/4P1M20AbLsRuODGw==";
            */
            
            identity.setValueForKeyPath(
                privKey, 
                "privKey"
            );
            identity.setValueForKeyPath(
                pubKey, 
                "pubKey"
            );
            var signer = net_dushin_crypto.SignerFactory.createSigner({privateKey: privKey});
            identity.setValueForKeyPath(
                signer, 
                "signer"
            );
        },
        100
    );
}

function addChannel(event)
{
    var channelName = prompt("Enter channel name:");
    if (channelName !== null) {
        Lethe.instance.joinChannel(channelName);
    }
}


function removeChannel(event)
{
    var channelList = dashcode.getDataSource("ChannelList");
    var channel = channelList.valueForKeyPath("selection");
    Lethe.instance.leaveChannel(channel);
}


function switchToPeersView(event)
{
    var layout = document.getElementById("ChannelStackLayout").object;
    layout.setCurrentView("PeersView");
}


function switchToMessagesView(event)
{
    var layout = document.getElementById("ChannelStackLayout").object;
    layout.setCurrentView("MessagesView");
}


messagesToText = Class.create(
    DC.ValueTransformer,
    {
        transformedValue: function(messages){
            var i;
            var s = "";
            for (i = 0;  i < messages.length; ++i) {
                var message = messages[i];
                /*
                var contents;
                if (message.isPlaintext()) {
                    
                }
                if (message.isEncrypted()) {
                }
                if (message.isSigned()) {
                }
                */
                s += message.toString() + '\n';
            }
            return s;
        }
    }
);



function sendMessage(event)
{
    var messageText = document.getElementById("MessageTextField").value;
    if (messageText.trim() === "") {
        return;
    }
    document.getElementById("MessageTextField").value = "";
    //
    //
    //
    var lethe = Lethe.instance;
    var channel = lethe.getSelectedChannel();
    var channelName = channel.valueForKeyPath('name');
    lethe.sendMessage(channelName, messageText);
    /*
    var channelList = dashcode.getDataSource("ChannelList");
    var messages = channelList.valueForKeyPath("selection.messages");
    
    var from = lethe.valueForKeyPath("content.identity.name");
    
    messages.addObject(
        // new Lethe.Message(
            {from: from, message: messageText}
        // )
    );
    */
}

mappubKeyToAnimating = Class.create(
    DC.ValueTransformer,
    {
        transformedValue: function(value){
            if (value === 'generating...') {
                return true;
            }
            return false;
        }
    }
);

pubKeyFingerprint = Class.create(
    DC.ValueTransformer,
    {
        transformedValue: function(value){
            if (value === 'generating...') {
                return '';
            }
            return net_dushin_crypto.KeyUtil.keyFingerprint(value);
        }
    }
);



function sendMessageOnEnter(event) {
    console.log(event);
    if (event.charCode === 13) {
        sendMessage(event);
    }
}


function modifyIdentity(event)
{
    var identity = Lethe.instance.getIdentity();
    var tmpIdentity = Lethe.instance.getTmpIdentity();
    tmpIdentity.name = identity.name;
    tmpIdentity.pubKey = identity.pubKey;
    tmpIdentity.privKey = identity.privKey;
    var layout = document.getElementById("MainStackLayout").object;
    layout.setCurrentView("EditIdentityView");
}


function editIdentityViewOk(event)
{
    var layout = document.getElementById("MainStackLayout").object;
    layout.setCurrentView("MainView");
    var lethe = Lethe.instance;
    var identity = lethe.getIdentity();
    var oldName = identity.getName();
    setTimeout(
        function() {
            var tmpIdentity = lethe.getTmpIdentity();
            identity.assign(tmpIdentity);
            lethe.setStoredIdentity(identity);
            lethe.updateIdentity(oldName, identity);
        },
        200
    );
}


function editIdentityViewCancel(event)
{
    var layout = document.getElementById("MainStackLayout").object;
    layout.setCurrentView("MainView");
}