FROM debian:bookworm-slim

# Install base dependencies and Guile/Guix
RUN apt-get update && apt-get install -y \
    curl build-essential guile-3.0 guile-3.0-dev git sudo wget locales xz-utils \
    && rm -rf /var/lib/apt/lists/*

# Set UTF-8 locale
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen

# Add user for devcontainer context
RUN useradd -ms /bin/bash dev && echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Install Guix binary (user-space) for reproducible package management
USER dev
WORKDIR /workspace

# Download, extract and install Guix binary with improved key authorization
RUN cd /tmp && \
    wget -q https://ftp.gnu.org/gnu/guix/guix-binary-1.4.0.x86_64-linux.tar.xz && \
    tar --warning=no-timestamp -xf guix-binary-1.4.0.x86_64-linux.tar.xz && \
    sudo mv var/guix /var/ && \
    sudo mv gnu / && \
    sudo ln -sf /var/guix/profiles/per-user/root/current-guix/bin/guix /usr/local/bin/guix && \
    sudo ln -sf /var/guix/profiles/per-user/root/current-guix/bin/guix-daemon /usr/local/bin/guix-daemon && \
    echo "Authorizing Guix public key..." && \
    (timeout 60 sh -c 'if [ -d "/gnu/store" ]; then key_file=$(find /gnu/store -name "ci.guix.gnu.org.pub" -type f -print -quit 2>/dev/null); if [ -n "$key_file" ] && [ -f "$key_file" ]; then echo "Found key: $key_file" && guix archive --authorize < "$key_file" && echo "✓ Key authorized successfully"; else echo "No public key found, skipping authorization"; fi; else echo "GNU store not found, skipping authorization"; fi' || echo "⚠️ Key authorization failed or timed out (build continues)") && \
    rm -rf /tmp/*

# Set up Guile environment
RUN echo 'export PATH="/home/dev/.guix-profile/bin:$PATH"' >> /home/dev/.bashrc && \
    echo 'export GUIX_LOCPATH="/home/dev/.guix-profile/lib/locale"' >> /home/dev/.bashrc && \
    echo 'source /home/dev/.guix-profile/etc/profile' >> /home/dev/.bashrc

# [Optional] Copy in a Wolfram installer script if you have licensing
COPY --chown=dev:dev wolfram-installer.sh /tmp/wolfram-installer.sh
RUN bash /tmp/wolfram-installer.sh || true

# Entrypoint is set by devcontainer.json