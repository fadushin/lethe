#
# Copyright (c) dushin.net
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of dushin.net nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

Dashcode Build Directory
------------------------

This directory contains scripts and files for building the Dashcode front ends
to various Lethe applications.  Because Dashcode does not support integration
with version control systems, we have written some scripts to generate
Dashcode project files out of artifacts from the Lethe source tree, and to
copy these files back into the source tree, when changes are made in
the Dashcode IDE.

In addition, because Lethe makes use of third party code that is incompatible with the
Dashcode framework, Dashcode needs to be patched to work with these libraries.
This directory contains a patch file, which can be applied to a COPY of your
Dashcode.app application.

Patching Dashcode 3.0.1
-----------------------

This step only needs to be performed once.

Take note of the Dashcode-3.0.1-tweaked.diff patch file in this directory.
This patch file makes modifications to a copy of the Dashcode IDE.

1. Create a COPY of the Dashcode.app IDE application, and place it in
a temporary directory.

IMPORTANT: You may want to re-name this application, so as not to confuse
it with your original Dashcode application, e.g., /tmp/Dashcode-tweaked.app.

2. Open a shell, and cd into the application directory (e.g., 
/tmp/Dashcode-tweaked.app), and issue:

    patch -p1 < .../Dashcode-3.0.1-tweaked.diff

where ... is the path to the Dashcode-3.0.1-tweaked.diff patch file.




