# SPDX-License-Identifier: PMPL-1.0-or-later
FROM gcr.io/oss-fuzz-base/base-builder-rust

COPY . $SRC/hypatia
WORKDIR $SRC/hypatia

COPY .clusterfuzzlite/build.sh $SRC/
