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

import net.dushin.lethe.messaging.client.ui.controller.Connection;
import net.dushin.lethe.messaging.client.ui.controller.Identity;
import net.dushin.lethe.messaging.client.ui.controller.LetheController;

public class
LetheWindow extends javax.swing.JFrame {

    public static final String TAG_HOST = "host";

    public static final String TAG_PORT = "port";

    public static final String TAG_IDENTITY = "identity";

    private static final long serialVersionUID = -4248702320880894936L;
    
    private final LetheController controller;
    
    private final LethePanel lethePanel;

    public
    LetheWindow(
        final java.util.Map<String, Object> config
    ) throws Exception {
        this(
            (String) config.get(TAG_HOST),
            (Short) config.get(TAG_PORT)
        );
    }
    
    private 
    LetheWindow(
        final String host, 
        final short port 
    ) throws Exception {
        super("Lethe Messaging Client");

        this.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
        
        setLayout(new java.awt.BorderLayout());
        this.controller = new LetheController(
            new Connection(host, port)
        );
        this.lethePanel = new LethePanel(controller);
        add("Center", this.lethePanel);
    }
    
    public LetheController
    getController() {
        return this.controller;
    }
    
    public javax.swing.JComponent
    getLethePanel() {
        return this.lethePanel;
    }

    public void setIdentity(Identity identity) {
        this.lethePanel.setIdentity(identity);
    }
}
