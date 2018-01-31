#!/bin/bash

# This helper script will run "npm install" in the directory it's in
# Useful because older versions of System.Process don't have nice things like readCreateProcess

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -e

cd $SCRIPTDIR
npm install
