#! /usr/bin/env sh

set -e

SCRIPT_DIR=`dirname $0 | sed -e "s|^\./|$PWD/|"`

cd "$SCRIPT_DIR/.."

sbt ++$TRAVIS_SCALA_VERSION scalariformFormat test:scalariformFormat
git diff --exit-code || (
  echo "ERROR: Scalariform check failed, see differences above."
  echo "To fix, format your sources using ./build scalariformFormat test:scalariformFormat before submitting a pull request."
  echo "Additionally, please squash your commits (eg, use git commit --amend) if you're going to update this pull request."
  false
)

echo "Update the validation resolvers"

# Sonatype staging (avoid Central sync delay)
perl -pe "s|resolvers |resolvers += \"Sonatype Staging\" at \"https://oss.sonatype.org/content/repositories/staging/\"\n\r\nresolvers |" < "$SCRIPT_DIR/../build.sbt" > /tmp/ReactiveMongo-Play-Json.scala && mv /tmp/ReactiveMongo-Play-Json.scala "$SCRIPT_DIR/../build.sbt"

if [ `sbt 'show version' 2>&1 | tail -n 1 | cut -d ' ' -f 2 | grep -- '-SNAPSHOT' | wc -l` -eq 1 ]; then
  perl -pe "s|resolvers |resolvers += \"Sonatype Snapshots\" at \"https://oss.sonatype.org/content/repositories/snapshots/\"\n\r\nresolvers |" < "$SCRIPT_DIR/../build.sbt" > /tmp/ReactiveMongo-Play-Json.scala && mv /tmp/ReactiveMongo-Play-Json.scala "$SCRIPT_DIR/../build.sbt"
fi

sbt ++$TRAVIS_SCALA_VERSION "testOnly *"
