libtoolize
aclocal -I m4
autoheader
automake -Wall --copy --add-missing
autoconf

