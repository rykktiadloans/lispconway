# Server-client Conway's game of life emulator written in Common Lisp

It does what it says on the tin. You instantiate a server, connect a client to it, and that's mostly it

## How to compile
1. Clone the repository
2. Compile the server using `make server` (if you have make) or `sbcl --load server.lisp --eval " (sb-ext:save-lisp-and-die \"server\" :toplevel #'main :executable t)" --disable-debugger`
3. Compile the client using `make client` (if you have make) or `sbcl --load client.lisp --eval " (sb-ext:save-lisp-and-die \"client\" :toplevel #'main :executable t)" --disable-debugger`
4. Run the executables (server has to be running first)
