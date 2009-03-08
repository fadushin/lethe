/**
 * Copyright (c) dushin.net
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of dushin.net nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package net.dushin.lethe.messaging.client.ui.components;

import net.dushin.lethe.messaging.client.ui.controller.LetheController;

class CryptoPanel extends javax.swing.JPanel {

    private final LetheController controller;

    private final IdentityPanel identityPanel;
    private final PeerPanel peerPanel;    
    
    CryptoPanel(
        final LetheController controller
    ) {
        this.controller = controller;
        //
        // create the panels
        //
        this.identityPanel = new IdentityPanel(this.controller);        
        this.peerPanel = new PeerPanel(this.controller);
        //
        // create a split pane for the panels
        //
        final javax.swing.JSplitPane splitPane = new javax.swing.JSplitPane(
            javax.swing.JSplitPane.VERTICAL_SPLIT,
            this.identityPanel,
            this.peerPanel
        );
        splitPane.setOneTouchExpandable(true);
        splitPane.setDividerLocation(150);
        //
        // add them to this panel
        //
        this.setLayout(new java.awt.BorderLayout());
        this.add("Center", splitPane);

    }
}
