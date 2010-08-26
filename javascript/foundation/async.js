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
// Convenience operations for calling functions asyncronously.
//
net_dushin_foundation.Async = {
    
    /**
     * Execute a function asynchrounously.
     *
     * @param       spec
     *              An object containing (possibly) the following properties:
     *                  * f (required): the function to call.  This function may 
     *                    take 0 or 1 arguments,
     *                    depending on the value of spec.args
     *                  * args (optional): the argument to pass to f
     *                  * resultCallback (optional): the function to call with the return
     *                    value from calling f.  The resultCallback takes one parameter, the
     *                    return value from calling f.
     *                  * exceptionCallback (optional): the function to call if the call to
     *                    f results in an exception.  The exceptionCallback takes one parameter,
     *                    the exception raised when calling f.
     *                  * timout (optional): the number of milliseconds to delay after
     *                    executing f.  (Default: 0)
     */
    exec: function(spec) {
        var f = spec.f;
        if (!f) {
            throw net_dushin_foundation.ExceptionFactory.createIllegalArgumentException(
                "net_dushin_foundation.Async.exec: Missing parameter: f"
            );
        }
        var args = spec.args;
        var resultCallback = spec.resultCallback;
        var exceptionCallback = spec.exceptionCallback;
        var timeout = spec.timeout ? spec.timeout : 0;
        setTimeout(
            function() {
                var result;
                try {
                    result = args ? f(args) : f();
                } catch (e) {
                    if (exceptionCallback) {
                        exceptionCallback(e);
                    } else {
                        console.error(e);
                    }
                    return;
                }
                if (resultCallback) {
                    resultCallback(result);
                }
            },
            timeout
        );
    }
};
