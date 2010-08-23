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

//
// startup/shutdown
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
                showChannelsView();
            } else {
                modifyIdentity();
            }
        }, 
        1000
    );
}

//
// Identity management
//


function modifyIdentity(event) {
    var identity = Lethe.instance.getIdentity();
    var tmpIdentity = Lethe.instance.getTmpIdentity();
    tmpIdentity.assign(identity);
    showIdentityView();
}

function generateRSAKey(event) {
    var model = Lethe.instance.getModel();
    model.setValueForKeyPath(0, "content.tmp.step");
    var progress = function(i) {
        if ((i % 10) === 0) {
            model.setValueForKeyPath(i, "content.tmp.step");
        }
    };

    Lethe.instance.regenerateTmpIdentityKey(progress);
}

function identityViewOk(event) {
    Lethe.instance.updateIdentity();
    showChannelsView();
    
}

function identityViewCancel(event) {
    showChannelsView();
}

//
// Channel Management
//

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

//
// Message Management
//

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
}

//
// Trusted Peer Management
//

function addTrustedPeer(event)
{
    var result = confirm("Are you sure you want to add this peer to the list of trusted peers?");
    if (result) {
        // TODO
    }
}



//
// View Selection/Change
//

function showIdentityView() {
    var layout = document.getElementById("LetheStack").object;
    layout.setCurrentView("IdentityView");
}

function showChannelsView() {
    var layout = document.getElementById("LetheStack").object;
    layout.setCurrentView("ChannelsView");
}

function showTrustedPeersView(event)
{
    var layout = document.getElementById("LetheStack").object;
    layout.setCurrentView("TrustedPeersView");
}

function showAboutView() {
    var layout = document.getElementById("LetheStack").object;
    layout.setCurrentView("AboutView");
}


function showPeersView(event)
{
    var layout = document.getElementById("ChannelStack").object;
    layout.setCurrentView("PeersView");
}

function showPeerDetailView(event)
{
    var layout = document.getElementById("ChannelStack").object;
    layout.setCurrentView("PeerDetailView");

}

function showMessagesView(event)
{
    var layout = document.getElementById("ChannelStack").object;
    layout.setCurrentView("MessagesView");
}


//
// Data transformers
//


pubKeyFingerprint = Class.create(
    DC.ValueTransformer,
    {
        transformedValue: function(value){
            if (value === Lethe.TAG_GENERATING) {
                return '';
            }
            return net_dushin_crypto.KeyUtil.keyFingerprint(value);
        }
    }
);


showGeneratingActivityMonitor = Class.create(
    DC.ValueTransformer,
    {
        transformedValue: function(value) {
            return value === Lethe.TAG_GENERATING;
        }
    }
);

messagesToText = Class.create(
    DC.ValueTransformer,
    {
        transformedValue: function(messages){
            var i;
            var s = "";
            for (i = 0;  i < messages.length; ++i) {
                var message = messages[i];
                if (message.asString) {
                    s += message.asString() + '\n';
                }
            }
            return s;
        }
    }
);

messagesToHtml = Class.create(
    DC.ValueTransformer,
    {
        transformedValue: function(messages){
            var i;
            var s = "";
            for (i = 0;  i < messages.length; ++i) {
                var message = messages[i];
                if (message.toHtml) {
                    if (i > 0) {
                        s += "<hr>";
                    }
                    s += message.toHtml(
                        "../Images/lethe/plaintext.png",
                        "../Images/lethe/encrypted.png",
                        "../Images/lethe/decrypted.png",
                        "../Images/lethe/unsigned.png",
                        "../Images/lethe/verified.png",
                        "../Images/lethe/unverified.png"
                    );
                }
            }
            return s;
        }
    }
);

editIdentityDoneButtonEnableable = Class.create(
    DC.ValueTransformer,
    {
        transformedValue: function(value){
            return value && value !== Lethe.TAG_GENERATING;
        }
    }
);

