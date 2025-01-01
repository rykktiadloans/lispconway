all: client.lisp server.lisp
	sbcl --load server.lisp --eval " (sb-ext:save-lisp-and-die \"server\" :toplevel #'main :executable t)" --disable-debugger
	sbcl --load client.lisp --eval " (sb-ext:save-lisp-and-die \"client\" :toplevel #'main :executable t)" --disable-debugger

client: client.lisp
	sbcl --load client.lisp --eval " (sb-ext:save-lisp-and-die \"client\" :toplevel #'main :executable t)" --disable-debugger

server: server.lisp
	sbcl --load server.lisp --eval " (sb-ext:save-lisp-and-die \"server\" :toplevel #'main :executable t)" --disable-debugger
