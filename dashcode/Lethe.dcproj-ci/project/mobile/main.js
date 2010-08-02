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
    Lethe.instance.regenerateTmpIdentityKey();
}

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


function modifyIdentity(event)
{
    var identity = Lethe.instance.getIdentity();
    var tmpIdentity = Lethe.instance.getTmpIdentity();
    tmpIdentity.assign(identity);
    var layout = document.getElementById("LetheStack").object;
    layout.setCurrentView("IdentityView");
}



function channelSelected(event)
{
    var list = document.getElementById("ChannelList").object;
    var browser = document.getElementById('LetheBrowser').object;
    var selectedObjects = list.selectedObjects();
    
    if (selectedObjects && (1 == selectedObjects.length)){
        // The Browser's goForward method is used to make the browser push down to a new level.
        // Going back to previous levels is handled automatically.
        browser.goForward(document.getElementById('MessageView'), selectedObjects[0].valueForKey("name"));
    }    
}


function modifyIdentitySelected(event)
{
    var browser = document.getElementById('LetheBrowser').object;
    browser.goForward(document.getElementById('IdentityView'), "Identity");
}


function showIdentityView(event)
{
    var stackLayout = document.getElementById('LetheStack').object;
    stackLayout.setCurrentView('IdentityView');
}


function showChannelsView(event)
{
    var stackLayout = document.getElementById('LetheStack').object;
    stackLayout.setCurrentView('ChannelsView');
}


showActivityMonitor = Class.create(DC.ValueTransformer,{
    transformedValue: function(value){
        return value === Lethe.TAG_GENERATING;
    }
});

