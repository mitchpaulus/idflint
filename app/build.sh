#!/bin/sh
# Publishes idflint as a Native AOT binary for the local platform.
# Cross-OS AOT publishing is not supported: run this script on Linux for
# linux-x64, on Windows for win-x64, on macOS for osx-x64/osx-arm64.
# CppCompilerAndLinker=cc uses the system C compiler (gcc or clang).
set -e

RID=${1:-linux-x64}
VERSION=$2

dotnet publish -c Release -r "$RID" \
    -p:PublishAot=true \
    -p:CppCompilerAndLinker=cc \
    -o "publish/$RID"

if [ -n "$VERSION" ]; then
    mkdir -p zips
    rm -f "zips/${RID}_v${VERSION}.zip"
    zip -j "zips/${RID}_v${VERSION}.zip" "publish/$RID"/idflint* "publish/$RID"/*.so 2>/dev/null || \
    zip -j "zips/${RID}_v${VERSION}.zip" "publish/$RID"/*
fi
