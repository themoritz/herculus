#!/bin/bash

set -e

# Server
cd server
stack build server
stack exec server-exe
