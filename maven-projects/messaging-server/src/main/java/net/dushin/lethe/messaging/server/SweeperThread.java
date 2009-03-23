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
package net.dushin.lethe.messaging.server;

import net.dushin.lethe.messaging.common.log.LogUtil;

class SweeperThread extends Thread {

    private static final java.util.logging.Logger LOGGER =
        java.util.logging.Logger.getLogger(SweeperThread.class.getName());
    
    private final ChannelManager mgr;

    SweeperThread(
        final ChannelManager mgr
    ) {
        this.mgr = mgr;
    }
    
    public void
    run() {
        while (true) {
            try {
                final java.util.Map<String, Channel> channelMap =
                    this.mgr.getChannelMap();
                synchronized (channelMap) {
                    for (final java.util.Map.Entry<String, Channel> entry : channelMap.entrySet()) {
                        final Channel channel = entry.getValue();
                        final int deltasecs = 
                            (int) (Timestamp.currentms() - channel.getLastTouched()) / 1000;
                        if (deltasecs > this.mgr.getMessagingServerConfig().getChannelIdleTimeoutSecs()) {
                            LogUtil.logInfo(
                                LOGGER, 
                                "Channel {0} timeout (after {1} secs); Removing channel...", 
                                channel.getId(), deltasecs
                            );
                            channelMap.remove(entry.getKey());
                        } else {
                            channel.sweepMessages();
                        }
                    }
                }
            } catch (final Throwable t) {
                LogUtil.logException(
                    LOGGER, 
                    java.util.logging.Level.WARNING, 
                    t, 
                    "An error occurred sweeping the set of channels."
                );
            } finally {
                try {
                    final int secs = 
                        this.mgr.getMessagingServerConfig().getSweeperThreadSleepSecs() * 1000;
                    LogUtil.logFine(LOGGER, "Sleeping for {0} secs...", secs);
                    Thread.sleep(secs);
                } catch (final InterruptedException e) {
                    // ignore
                }
            }
        }
    }
}
