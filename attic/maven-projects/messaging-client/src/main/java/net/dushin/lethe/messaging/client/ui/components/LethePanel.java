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

import net.dushin.lethe.messaging.client.ui.controller.Identity;
import net.dushin.lethe.messaging.client.ui.controller.LetheController;

public class LethePanel extends javax.swing.JPanel {

    private static final long serialVersionUID = -3361833391155415546L;

    private final LetheController controller;
    
    private final IdentityPanel identityPanel;
    private final TabbedChannelPanel messagePanel;
    
    public 
    LethePanel(
        final LetheController controller
    ) {
        this.controller = controller;

        setLayout(new java.awt.BorderLayout());
        
        this.identityPanel = new IdentityPanel(this.controller);
        javax.swing.JPanel statusPanel = new StatusPanel(this.controller);

        final javax.swing.JSplitPane statusplitPane = new javax.swing.JSplitPane(
            javax.swing.JSplitPane.HORIZONTAL_SPLIT,
            this.identityPanel,
            statusPanel
        );
        statusplitPane.setOneTouchExpandable(true);
        // statusplitPane.setDividerLocation(350);
        
        this.messagePanel = new TabbedChannelPanel(controller);
        
        final javax.swing.JSplitPane cryptoMessageSplitPane = new javax.swing.JSplitPane(
            javax.swing.JSplitPane.VERTICAL_SPLIT,
            statusplitPane,
            this.messagePanel
        );
        cryptoMessageSplitPane.setOneTouchExpandable(true);
        // cryptoMessageSplitPane.setDividerLocation(250);
        
        this.add("Center", cryptoMessageSplitPane);
    }
    
    public void
    createTabbedPane(
        final String channel
    ) {
        this.messagePanel.createTabbedPane(channel);
    }

    public void setIdentity(Identity identity) {
        this.identityPanel.setIdentity(identity);
    }
    
}
