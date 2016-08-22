#!/bin/bash

set -e

# Client
cd client && stack build client && cd ..
