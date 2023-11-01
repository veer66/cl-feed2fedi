#!/bin/bash

docker run --rm -it -v $HOME/cl-feed2fedi-data:/work/data docker.io/veer66/cl-feed2fedi sbcl \
	--eval '(ql:quickload "cl-feed2fedi")' \
	--eval '(cl-feed2fedi:main)'


