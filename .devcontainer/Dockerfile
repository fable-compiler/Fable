# We use a custom dev container for Fable, because we want to be able to
# control the version of each target language we use.
# This also allow to have a consistent development environment for
# contributors and avoid problems like having some tools not compilable
# because of .NET specificities on OSX ARM for example.

# To debug this DockerFile, you can use the following commands:
#
# Should be run from the .devcontainer folder
#
# Build the docker file with the fable-dev tag
#   docker build --tag fable-dev .
# or if you want to see the progress (useful for debugging)
#   docker build --tag fable-dev . --progress=plain
#
# Run the docker file and delete it on exit:
#   docker run -it --rm fable-dev

# Use x86_64 architecture to avoid all the small pains
# coming from alpine over ARM --platform=linux/amd64
FROM mcr.microsoft.com/devcontainers/base:debian
# Install the xz-utils package
RUN apt-get update && apt-get install -y xz-utils

# Options
# Let it here to avoid conflicts with Microsoft options !!!
ARG DOTNET_VERSION=8.0.100
ARG NODE_VERSION=18.8.0
ARG PYTHON_VERSION=3.11.5
ARG DART_VERSION=3.1.2

USER vscode

# Change the default shell to zsh
SHELL ["/bin/zsh", "-c"]

# # Install .NET
WORKDIR /home/vscode
RUN sudo apt install wget
RUN wget -qO- https://dot.net/v1/dotnet-install.sh | bash -s - --version $DOTNET_VERSION
RUN echo "# Load dotnet path" >> .zshrc
RUN echo "export PATH=$PATH:/home/vscode/.dotnet" >> .zshrc
# Trigger the dotnet first run experience by running a command
RUN source .zshrc && dotnet --help
# Add dotnet zsh completion
COPY scripts/zsh_dotnet_completion.sh /home/vscode/zsh_dotnet_completion.sh
RUN cat /home/vscode/zsh_dotnet_completion.sh >> .zshrc
RUN rm /home/vscode/zsh_dotnet_completion.sh

# Install Node.js
RUN wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash
# Even, if we changed the default shell to ZSH, nvm install script
# still uses bash, so we manually need to add the nvm path to zsh
RUN echo "# Load nvm path" >> .zshrc
RUN echo 'export NVM_DIR="$HOME/.nvm"' >> .zshrc
RUN echo '[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm' >> .zshrc
RUN echo '[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion"' >> .zshrc
# We need to source the zshrc and execute nvm install in the same RUN command
# Otherwise, the nvm command will not be available
RUN source .zshrc && nvm install $NODE_VERSION

# Install python
# Install Python build dependencies
RUN sudo apt-get install -y build-essential libssl-dev zlib1g-dev \
libbz2-dev libreadline-dev libsqlite3-dev curl \
libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev
# Install pyenv
RUN curl https://pyenv.run | bash
RUN echo "# Load pyenv path" >> .zshrc
RUN echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.zshrc
RUN echo 'command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.zshrc
RUN echo 'eval "$(pyenv init -)"' >> ~/.zshrc
RUN source .zshrc && pyenv install $PYTHON_VERSION
# Make python point to the installed version
RUN source .zshrc && pyenv global $PYTHON_VERSION
RUN source .zshrc && curl -sSL https://install.python-poetry.org | python
RUN echo "# Load poetry path" >> .zshrc
RUN echo 'export PATH="$HOME/.local/bin:$PATH"' >> .zshrc

# Install rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s - -y
RUN echo "# Load rust path" >> .zshrc
RUN echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> .zshrc

# Instal dart
# Use custom dsm install script, because we broke it while
#  trying to fix another issue with dsm
# See https://github.com/Yakiyo/dsm/issues/26
COPY scripts/install_dsm.sh /home/vscode/install_dsm.sh
RUN sudo chmod +x /home/vscode/install_dsm.sh
RUN /home/vscode/install_dsm.sh
# RUN curl -fsSL https://dsm-vm.vercel.app/install.sh | bash
RUN echo "# Load dart path" >> .zshrc
RUN echo 'export PATH="/home/vscode/.dsm:$PATH"' >> .zshrc
RUN echo 'eval "`dsm env zsh`"' >> .zshrc
# Workaround dsm bug (see script for more details)
COPY scripts/install_dart.sh /home/vscode/install_dart.sh
RUN sudo chmod +x /home/vscode/install_dart.sh
RUN source .zshrc && /home/vscode/install_dart.sh $DART_VERSION
RUN source .zshrc && dsm use $DART_VERSION
RUN rm /home/vscode/install_dsm.sh
RUN rm /home/vscode/install_dart.sh

# Force docker to load the zsh profile
# This should be the last steps
CMD [ "/bin/zsh" ]
