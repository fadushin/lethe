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

class PeerTableModel extends javax.swing.table.AbstractTableModel {

    private static final String COLUMN_NAMES[] = {
        "Encrypt",
        "Peer Name"
    };
    
    private final LetheController controller;
    
    PeerTableModel(
        final LetheController controller
    ) {
        this.controller = controller;
    }
    
    //
    // javax.swing.table.TableModel implementation
    //
    
    public final int 
    getColumnCount() {
        return COLUMN_NAMES.length;
    }

    public final int 
    getRowCount() {
        return this.controller.getPeers().size();
    }

    public final String 
    getColumnName(int col) {
        return COLUMN_NAMES[col];
    }

    public final Object 
    getValueAt(
        final int row, 
        final int col
    ) {
        switch (col) {
        case 0:
            return this.controller.getPeers().get(row).getEncryptTo();
        case 1:
            return this.controller.getPeers().get(row).getName();
        default:
            assert false;
            throw new RuntimeException("unsupported col num" + col);
        }
    }

    public final Class 
    getColumnClass(
        final int col
    ) {
        return getValueAt(0, col).getClass();
    }
    
    public final boolean 
    isCellEditable(
        final int row, 
        final int col
    ) {
        return col == 0;
    }
    
    public final void 
    setValueAt(
        final Object value, 
        final int row, 
        final int col
    ) {
        System.out.println(value);
        this.controller.getPeers().get(row).setEncryptTo((Boolean) value);
        fireTableCellUpdated(row, col);
    }
}
