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
// An ExceptionFactory is used to create a exception objects.
//
net_dushin_exception.ExceptionFactory = {

    ILLEGAL_ARGUMENT: "ILLEGAL_ARGUMENT";

    //
    // object create(spec)
    //
    // @param       spec
    //              The constructor specification.  This parameter may contain
    //              the following elements:
    //
    //                  * message:        the exception message
    //
    createException: function(spec) {
        //
        // the spec (possibly undefined)
        //
        var myspec = spec ? spec : {};
        //
        // return the exception
        //
        return {
            //
            // boolean getType()
            //
            // @return      the exception type, or undefined, if the type
            //              is not specified
            //
            getType: function() {
                return myspec.type;
            }
            //
            // boolean getMessage()
            //
            // @return      the exception message, or undefined, if the message
            //              is not specified
            //
            getMessage: function() {
                return myspec.message;
            }
        };
    }
    
    createIllegalArgumentException: function(spec) {
        spec.type = ILLEGAL_ARGUMENT;
        return create(spec);
    }
};

