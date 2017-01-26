#!/bin/bash

set -e

# Server
stack build server
stack exec server-exe
