#!/bin/sh -x

dotnet tool restore
dotnet run -- $@