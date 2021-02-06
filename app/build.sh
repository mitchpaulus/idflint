#!/bin/sh
dotnet publish -r win-x64   -c Release --self-contained
dotnet publish -r linux-x64 -c Release --self-contained
dotnet publish -r osx-x64   -c Release --self-contained

rm -f zips/win-x64.zip
rm -f zips/linux-x64.zip
rm -f zips/osx-x64.zip

zip -j zips/win-x64.zip bin/Release/netcoreapp3.1/win-x64/publish/*
zip -j zips/linux-x64.zip bin/Release/netcoreapp3.1/linux-x64/publish/*
zip -j zips/osx-x64.zip bin/Release/netcoreapp3.1/osx-x64/publish/*
