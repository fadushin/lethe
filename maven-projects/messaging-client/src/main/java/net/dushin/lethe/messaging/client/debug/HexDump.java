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
package net.dushin.lethe.messaging.client.debug;

public final class HexDump {

    private static final char[] HEX_TABLE = new char[] {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 
        'A', 'B', 'C', 'D', 'E', 'F'
    };
    
    private static final String NL = System.getProperty("line.separator");
    
    private static final int NUM_COLS = 4;
    private static final int BYTES_PER_COL = 4;

    private
    HexDump() {
        // complete
    }

    public static String
    dump(
        final byte[] data
    ) {
        final StringBuilder buf = new StringBuilder();
        final int dataLen = data.length;
        final int numRows = dataLen / (NUM_COLS * BYTES_PER_COL);
        for (int col = 0;  col < numRows;  ++col) {
            buf.append(dumpRow(data, col * NUM_COLS * BYTES_PER_COL, NUM_COLS * BYTES_PER_COL));
        }
        buf.append(dumpRow(data, numRows * NUM_COLS * BYTES_PER_COL, dataLen % (NUM_COLS * BYTES_PER_COL)));
        return buf.toString();
    }
    
    private static String
    dumpRow(
        final byte[] data,
        final int offset,
        final int len
    ) {
        final StringBuilder linBuf = new StringBuilder();
        final StringBuilder hexBuf = new StringBuilder();
        final StringBuilder binBuf = new StringBuilder();
        //
        // write out the 2 sets of columns
        //
        linBuf.append(toHex((short) offset));
        for (int i = 0;  i < len;  ++i) {
            if (i != 0 && i % BYTES_PER_COL == 0) {
                hexBuf.append(' ');
                binBuf.append(' ');
            }
            hexBuf.append(toHex(data[offset + i]));
            binBuf.append(toBin(data[offset + i]));
        }
        //
        // pad the remainder
        //
        for (int i = len;  i < NUM_COLS * BYTES_PER_COL;  ++i) {
            if (i != 0 && i % BYTES_PER_COL == 0) {
                hexBuf.append(' ');
                binBuf.append(' ');
            }
            hexBuf.append("  ");
            binBuf.append(' ');
        }
        //
        // catenate the sets
        //
        final StringBuilder buf = linBuf;
        buf.append(": ");
        buf.append(hexBuf.toString());
        buf.append(" | ");
        buf.append(binBuf.toString());
        buf.append(NL);
        return buf.toString();
    }
    
    public static String
    toHex(
        final short n
    ) {
        final StringBuilder buf = new StringBuilder();
        for (int i = 3;  i >= 0;  --i) {
            final byte b = (byte) ((n >>> (i * 8)) & 0xFF);
            buf.append(toHex(b));
        }
        return buf.toString();
    }
    
    public static String
    toHex(
        final byte b
    ) {
        final StringBuilder buf = new StringBuilder();
        for (int i = 1;  i >= 0;  --i) {
            final byte bx = (byte) ((b >> (i * 4)) & 0x0F);
            buf.append(HEX_TABLE[bx]);
        }
        return buf.toString();
    }
    
    private static char
    toBin(
        final byte b
    ) {
        if (0x1F < b && b < 0x7F) {
            return (char) b;
        } else {
            return '.';
        }
    }
}
