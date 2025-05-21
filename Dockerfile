FROM elixir:1.18

# Set working directory
WORKDIR /app

# Configure apt sources - create if needed
# Replace the current Step 3 with this:
RUN echo "deb http://deb.debian.org/debian bookworm main" > /etc/apt/sources.list && \
    echo "deb http://deb.debian.org/debian bookworm-updates main" >> /etc/apt/sources.list && \
    echo "deb http://security.debian.org/debian-security bookworm-security main" >> /etc/apt/sources.list

# Install dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    git \
    python3 \
    curl \
    ca-certificates && \
    # Install Node.js via NodeSource
    curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs && \
    # Install npm and update it
    npm install -g npm@latest && \
    # Clean up
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Set build environment
ENV MIX_ENV=prod

# Install Hex + Rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Copy and install dependencies
COPY mix.exs mix.lock ./
RUN mix deps.get --only prod && \
    mix deps.compile

# Install NPM dependencies and build assets
COPY assets assets
RUN cd assets && \
    npm install && \
    npm run deploy

# Copy remaining files
COPY priv priv
COPY lib lib

# Compile the project
RUN mix compile

# Create uploads directory
RUN mkdir -p /app/bin/uploads

# Generate release
RUN mix phx.digest && \
    mix release

# Set server environment
ENV PHX_SERVER=true

# Run the server
CMD ["_build/prod/rel/mazaryn/bin/mazaryn", "start"]
