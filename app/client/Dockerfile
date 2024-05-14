FROM node:14

# System dependencies
RUN apt-get update && apt-get install -y libncurses5 zopfli

# Download and install purescript
RUN wget https://github.com/purescript/purescript/releases/download/v0.11.7/linux64.tar.gz
RUN tar -xvf linux64.tar.gz
RUN mv purescript/* /usr/local/bin

# Create build directory
RUN mkdir -p /opt/build
WORKDIR /opt/build

# Install npm/bower dependencies
COPY package.json package-lock.json bower.json ./
RUN npm install
RUN ./node_modules/bower/bin/bower install

# Copy source code
COPY . /opt/build

# Build the project
ARG API_URL=https://app.herculus.io/api/
ARG WEBSOCKET_URL=wss://app.herculus.io/websocket
RUN npm run build-prod
RUN zopfli --i15 public/bundle-*.js
