#!/bin/sh

nargs=$#

if [ "${nargs}" -lt "2" ]
then
    echo "Syntax: $0 <old-version> <new-version>"
    exit 1
fi
old_version=$1
new_version=$2

for i in $(find . -name pom.xml)
do
    echo "updating version in $i from ${old_version} to ${new_version}..."
    replace ${old_version} ${new_version} $i
done
