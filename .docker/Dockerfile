FROM gitpod/workspace-full:latest

USER gitpod

# Install .NET Core 3.1 SDK binaries on Ubuntu 20.04
# Configuration adapted from: https://github.com/gitpod-io/workspace-images/blob/197b9ad25ed4b4af2b07facb62518f52d7715a07/dotnet/Dockerfile
# In order to get the download link:
# 1. Go to: https://dotnet.microsoft.com/download/dotnet-core
# 2. Select the version you want to download
# 3. Choose "Binaries" x64
# 4. In the new tab close the popup window
# 5. Copy the "Direct link" shown on the page and replace the URL in this file
RUN mkdir -p /home/gitpod/dotnet && curl -fsSL https://download.visualstudio.microsoft.com/download/pr/0c795076-b679-457e-8267-f9dd20a8ca28/02446ea777b6f5a5478cd3244d8ed65b/dotnet-sdk-3.1.300-linux-x64.tar.gz | tar xz -C /home/gitpod/dotnet
ENV DOTNET_ROOT=/home/gitpod/dotnet
ENV PATH=$PATH:/home/gitpod/dotnet
