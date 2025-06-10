FROM debian:bookworm-slim

# Install base dependencies and Guile/Guix
RUN apt-get update && apt-get install -y \
    curl build-essential guile-3.0 guile-3.0-dev git sudo wget locales xz-utils \
    gpg gpg-agent coreutils grep sed tar gawk which \
    && rm -rf /var/lib/apt/lists/*

# Set UTF-8 locale
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen

# Add user for devcontainer context
RUN useradd -ms /bin/bash dev && echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Install Guix using the official installation script
COPY etc_guix-install.sh /tmp/etc_guix-install.sh
RUN chmod +x /tmp/etc_guix-install.sh && \
    # Run the script in non-interactive mode by providing empty input
    # Make installation optional to avoid blocking prebuild on network issues
    echo '' | /tmp/etc_guix-install.sh || echo "Guix installation failed, continuing without it"

# Switch to dev user and set up environment
USER dev
WORKDIR /workspace

# Set up Guile environment for dev user
RUN echo 'export PATH="/home/dev/.guix-profile/bin:$PATH"' >> /home/dev/.bashrc && \
    echo 'export GUIX_LOCPATH="/home/dev/.guix-profile/lib/locale"' >> /home/dev/.bashrc && \
    echo 'source /home/dev/.guix-profile/etc/profile' >> /home/dev/.bashrc

# [Optional] Copy in a Wolfram installer script if you have licensing
COPY --chown=dev:dev wolfram-installer.sh /tmp/wolfram-installer.sh
RUN bash /tmp/wolfram-installer.sh || true

# Entrypoint is set by devcontainer.json