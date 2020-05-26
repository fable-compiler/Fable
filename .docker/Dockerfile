FROM gitpod/workspace-full:latest

USER gitpod

# Install .NET Core 3.1 SDK binaries on Ubuntu 20.04
# Source: https://dev.to/carlos487/installing-dotnet-core-in-ubuntu-20-04-6jh
RUN mkdir -p /home/gitpod/dotnet && curl -fsSL https://download.visualstudio.microsoft.com/download/pr/0c795076-b679-457e-8267-f9dd20a8ca28/02446ea777b6f5a5478cd3244d8ed65b/dotnet-sdk-3.1.300-linux-x64.tar.gz | tar xz -C /home/gitpod/dotnet
ENV DOTNET_ROOT=/home/gitpod/dotnet
ENV PATH=$PATH:/home/gitpod/dotnet
