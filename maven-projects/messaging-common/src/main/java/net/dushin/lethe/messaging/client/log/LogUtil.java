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
package net.dushin.lethe.messaging.client.log;

import net.dushin.lethe.messaging.client.debug.HexDump;

/**
 * 
 */
public abstract class LogUtil {

    /**
     * The Logger instance used by this class
     */
    private static final java.util.logging.Logger LOGGER =
        java.util.logging.Logger.getLogger(LogUtil.class.getName());
    
    /**
     * the log level at which we log buffers
     */
    private static final java.util.logging.Level DUMP_LEVEL =
        java.util.logging.Level.FINE;
    
    /**
     * the newline chars for this platform
     */
    private static final String NL = System.getProperty("line.separator");
    
    private
    LogUtil() {
    }
    
    public static void
    logInfo(
        final java.util.logging.Logger logger,
        final String pattern,
        final Object... params
    ) {
        log(logger, java.util.logging.Level.INFO, pattern, params);
    }
    
    public static void
    logException(
        final java.util.logging.Logger logger,
        final Exception e,
        final String pattern,
        final Object... params
    ) {
        logException(logger, java.util.logging.Level.WARNING, e, pattern, params);
    }
    
    public static void
    logException(
        final java.util.logging.Logger logger,
        final java.util.logging.Level level,
        final Exception e,
        final String pattern,
        final Object... params
    ) {
        log(logger, level, e, pattern, params);
    }

    public static void
    logBuffer(
        final String header,
        final byte[] buffer
    ) {
        logBuffer(LOGGER, header, buffer);
    }

    public static void
    logBuffer(
        final java.util.logging.Logger logger,
        final String header,
        final byte[] buffer
    ) {
        if (logger.isLoggable(DUMP_LEVEL)) {
            logger.log(
                DUMP_LEVEL,
                header + NL
                + "Buffer size (bytes): " + buffer.length + NL
                + HexDump.dump(buffer)
            );
        }
    }
    
    private static void
    log(
        final java.util.logging.Logger logger,
        final java.util.logging.Level level,
        final String pattern,
        final Object... params
    ) {
        log(logger, level, null, pattern, params);
    }
    
    private static void
    log(
        final java.util.logging.Logger logger,
        final java.util.logging.Level level,
        final Exception e,
        final String pattern,
        final Object... params
    ) {
        if (logger.isLoggable(level)) {
            //
            // TODO localization would go hereabouts
            //
            String msg = java.text.MessageFormat.format(pattern, params);
            if (e != null) {
                msg += NL + "The offending exception is:" + NL + generateStackTrace(e);
            }
            final String[] location = inferCaller();
            logger.logp(level, location[0], location[1], msg);
        }
    }
    
    private static String
    generateStackTrace(
        final Exception e
    ) {
        final java.io.ByteArrayOutputStream os = new java.io.ByteArrayOutputStream();
        final java.io.PrintWriter s = new java.io.PrintWriter(os);
        e.printStackTrace(s);
        s.flush();
        return os.toString();
    }
    
    private static String[]
    inferCaller() {
        final StackTraceElement stack[] = (new Throwable()).getStackTrace();
        int idx = 0;
        while (idx < stack.length) {
            final StackTraceElement frame = stack[idx];
            final String cname = frame.getClassName();
            if (cname.equals(LogUtil.class.getName())) {
                break;
            }
            idx++;
        }
        while (idx < stack.length) {
            StackTraceElement frame = stack[idx];
            String cname = frame.getClassName();
            if (!cname.equals(LogUtil.class.getName())) {
                return new String[]{cname, frame.getMethodName()};
            }
            idx++;
        }
        return new String[]{"unknown-class", "unknown-method"};
    }
}
