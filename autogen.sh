#!/bin/sh
ln -fs README.md README
autoreconf -vfi && \
autoconf && {
  echo "Running configure with --enable-maintainer-mode $@"
  ./configure --enable-maintainer-mode $@
}
