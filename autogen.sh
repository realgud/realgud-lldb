#!/bin/sh
ln -fs README.md README
touch common.mk
autoreconf -vfi && \
autoconf && {
  echo "Running configure with --enable-maintainer-mode $@"
  ./configure --enable-maintainer-mode $@
}
