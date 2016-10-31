#!/bin/bash

set -ev

# Build if TRAVIS_TAG is unset or empty.
[ -n "${TRAVIS_TAG}" ] && exit 0;

# Build if there is no tag for the current commit.
t=$(git name-rev --tags --name-only $(git rev-parse HEAD))
[ -n "$t" ] && exit 0;

[ ${TRAVIS_SECURE_ENV_VARS} == false ] && exit -1;

openssl aes-256-cbc -pass pass:$ENCRYPTION_PASSWORD -in secring.gpg.enc -out local.secring.gpg -d
openssl aes-256-cbc -pass pass:$ENCRYPTION_PASSWORD -in pubring.gpg.enc -out local.pubring.gpg -d
openssl aes-256-cbc -pass pass:$ENCRYPTION_PASSWORD -in credentials.sbt.enc -out local.credentials.sbt -d
openssl aes-256-cbc -pass pass:$ENCRYPTION_PASSWORD -in deploy_key.enc -out local.deploy_key -d

sbt -jvm-opts travis/jvmopts.compile compile test
