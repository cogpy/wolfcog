FROM debian:bookworm-slim

# Install Guix and dependencies
RUN apt-get update && \
    apt-get install -y curl build-essential git wget xz-utils sudo && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Create dev user
RUN useradd -ms /bin/bash dev && \
    echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Install Guix binary
USER dev
WORKDIR /home/dev

RUN cd /tmp && \
    wget -q https://ftp.gnu.org/gnu/guix/guix-binary-1.4.0.x86_64-linux.tar.xz && \
    tar --warning=no-timestamp -xf guix-binary-1.4.0.x86_64-linux.tar.xz && \
    sudo mv var/guix /var/ && \
    sudo mv gnu / && \
    sudo ln -sf /var/guix/profiles/per-user/root/current-guix/bin/guix /usr/local/bin/guix && \
    sudo ln -sf /var/guix/profiles/per-user/root/current-guix/bin/guix-daemon /usr/local/bin/guix-daemon && \
    sudo guix archive --authorize < /gnu/store/*/share/guix/ci.guix.gnu.org.pub && \
    rm -rf /tmp/*

# Set up Guile environment
RUN echo 'export PATH="/home/dev/.guix-profile/bin:$PATH"' >> /home/dev/.bashrc && \
    echo 'export GUIX_LOCPATH="/home/dev/.guix-profile/lib/locale"' >> /home/dev/.bashrc && \
    echo 'source /home/dev/.guix-profile/etc/profile' >> /home/dev/.bashrc

# Optionally add Wolfram Engine if available
COPY --chown=dev:dev wolfram-installer.sh /tmp/wolfram-installer.sh
RUN bash /tmp/wolfram-installer.sh || true

USER dev
WORKDIR /workspace