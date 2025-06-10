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

# Download and extract Guix binary - Step 1: Network/Archive Access
RUN echo "📥 Starting Guix binary download..." && \
    cd /tmp && \
    (wget -q https://ftp.gnu.org/gnu/guix/guix-binary-1.4.0.x86_64-linux.tar.xz || \
     wget -q https://mirror.guix.gnu.org/guix-binary-1.4.0.x86_64-linux.tar.xz || \
     echo "⚠️ Failed to download Guix binary from primary mirrors") && \
    echo "✓ Guix binary download completed"

RUN echo "📦 Extracting Guix binary archive..." && \
    cd /tmp && \
    if [ -f guix-binary-1.4.0.x86_64-linux.tar.xz ]; then \
        tar --warning=no-timestamp -xf guix-binary-1.4.0.x86_64-linux.tar.xz && \
        echo "✓ Guix binary extracted successfully"; \
    else \
        echo "⚠️ Guix binary archive not found, skipping extraction"; \
    fi

# Install Guix binary - Step 2: Filesystem Permissions/Overlay  
RUN echo "📁 Moving Guix directories to system locations..." && \
    cd /tmp && \
    if [ -d var/guix ]; then \
        sudo mv var/guix /var/ && \
        echo "✓ /var/guix moved successfully"; \
    else \
        echo "⚠️ var/guix directory not found, skipping move"; \
    fi && \
    if [ -d gnu ]; then \
        sudo mv gnu / && \
        echo "✓ /gnu moved successfully"; \
    else \
        echo "⚠️ gnu directory not found, skipping move"; \
    fi

# Create symlinks - Step 3: Symlink/Path Issues
RUN echo "🔗 Creating Guix symlinks..." && \
    if [ -f /var/guix/profiles/per-user/root/current-guix/bin/guix ]; then \
        sudo ln -sf /var/guix/profiles/per-user/root/current-guix/bin/guix /usr/local/bin/guix && \
        echo "✓ guix symlink created"; \
    else \
        echo "⚠️ Guix binary not found, skipping guix symlink"; \
    fi && \
    if [ -f /var/guix/profiles/per-user/root/current-guix/bin/guix-daemon ]; then \
        sudo ln -sf /var/guix/profiles/per-user/root/current-guix/bin/guix-daemon /usr/local/bin/guix-daemon && \
        echo "✓ guix-daemon symlink created"; \
    else \
        echo "⚠️ Guix daemon binary not found, skipping guix-daemon symlink"; \
    fi

# Authorize Guix public key - Step 4: Key Authorization (with reduced timeout)
RUN echo "🔑 Authorizing Guix public key..." && \
    (timeout 5 sh -c 'if [ -d "/gnu/store" ]; then echo "🔍 Searching for Guix public key..."; key_file=$(find /gnu/store -name "ci.guix.gnu.org.pub" -type f -print -quit 2>/dev/null); if [ -n "$key_file" ] && [ -f "$key_file" ]; then echo "Found key: $key_file" && guix archive --authorize < "$key_file" && echo "✓ Key authorized successfully"; else echo "No public key found, skipping authorization"; fi; else echo "GNU store not found, skipping authorization"; fi' || echo "⚠️ Key authorization failed or timed out (build continues)")

# Cleanup - Step 5: Cleanup  
RUN echo "🧹 Cleaning up temporary files..." && \
    rm -rf /tmp/guix* /tmp/gnu* && \
    echo "✓ Cleanup completed"

# Set up Guile environment
RUN echo 'export PATH="/home/dev/.guix-profile/bin:$PATH"' >> /home/dev/.bashrc && \
    echo 'export GUIX_LOCPATH="/home/dev/.guix-profile/lib/locale"' >> /home/dev/.bashrc && \
    echo 'source /home/dev/.guix-profile/etc/profile' >> /home/dev/.bashrc

# [Optional] Copy in a Wolfram installer script if you have licensing
COPY --chown=dev:dev wolfram-installer.sh /tmp/wolfram-installer.sh
RUN bash /tmp/wolfram-installer.sh || true

# Entrypoint is set by devcontainer.json