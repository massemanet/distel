#!/bin/sh

function usage () {
    echo "$0 major|minor|patch"
    exit
}

OVSN=$(git describe  | cut -f1 -d"-")
[ -z "$OVSN" ] && usage

if [ -z "$1" ]; then
    size="patch"
else
    size=$1
fi

MAJOR=`echo $OVSN | cut -f1 -d"."`
MINOR=`echo $OVSN | cut -f2 -d"."`
PATCH=`echo $OVSN | cut -f3 -d"."`
if [ "$size" == "major" ]; then
    NVSN=$(($MAJOR + 1)).0.0
elif [ "$size" == "minor" ]; then
    NVSN=$MAJOR.$(($MINOR + 1)).0
elif [ "$size" == "patch" ]; then
    NVSN=$MAJOR.$MINOR.$(($PATCH + 1))
else
    usage
fi

echo $OVSN"->"$NVSN
echo -e "Version $NVSN. "`date`"\n" | cat - NEWS > /tmp/$$ && mv /tmp/$$ NEWS
git add NEWS
git commit -m"v$NVSN"
git log --name-only --no-merges | grep -Ev '^[ ]+$$|git-svn-id' > ChangeLog
echo " amongst others:" > AUTHORS
echo " mats cronqvist (maintainer) <masse@cronqvi.st>" >> AUTHORS
echo " Martin Bjorklund <mbj <at> tail-f.com>" >> AUTHORS
echo " david wallin <david.wallin@ul.ie>" >> AUTHORS
git log | grep Author | grep -Evi "vagrant|no author|ronqvist" | sort -u | cut -c8- >> AUTHORS
git add ChangeLog AUTHORS
git commit --amend --reuse-message HEAD
git tag -a -m"$NVSN" $NVSN
#git push --tags
