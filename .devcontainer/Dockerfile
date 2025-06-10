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
    # Patch the script to support non-interactive mode
    # This adds a check at the beginning of the welcome() function
    # to return early if NONINTERACTIVE=1 is set
    awk '
    /^welcome\(\)$/ { in_welcome = 1; print; next }
    in_welcome && /^{$/ { 
        print
        print "    # Return early if NONINTERACTIVE=1 is set"
        print "    if [ \"$NONINTERACTIVE\" = \"1\" ]; then"
        print "        return"
        print "    fi"
        next
    }
    in_welcome && /^}$/ { in_welcome = 0 }
    { print }
    ' /tmp/etc_guix-install.sh > /tmp/etc_guix-install-patched.sh && \
    mv /tmp/etc_guix-install-patched.sh /tmp/etc_guix-install.sh && \
    chmod +x /tmp/etc_guix-install.sh && \
    # Run the script in non-interactive mode
    NONINTERACTIVE=1 /tmp/etc_guix-install.sh

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