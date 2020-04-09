FROM gitpod/workspace-full:latest

USER root

# Add here the instruction who needs root user


USER gitpod

# Install dotnet core
ENV PATH=$HOME/.dotnet:$PATH
RUN wget https://dotnetwebsite.azurewebsites.net/download/dotnet-core/scripts/v1/dotnet-install.sh && \
    bash dotnet-install.sh -v 3.1.201 && \
    rm -rf dotnet-install.sh;
