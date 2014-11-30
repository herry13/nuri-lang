#!/bin/bash

BASEDIR="$(dirname $0)"
CURRENTDIR=$(pwd)
OPT_TYPE="-t"
#OPT_YAML="-yaml"
OPT_AST="-x"
#OPT_AST_JSON="-ast-json"
NURIC="$BASEDIR/../ocaml/nuric"
EXT="nuri"

JSON2NURI="$BASEDIR/../utils/json2nuri.rb"

red='\x1B[0;31m'
green='\x1B[0;32m'
nocolor='\x1B[0m'

function test {
    if [[ -f $1 && "${1##*.}" = $EXT ]]; then
        if [ $# == 2 ]; then
            message=$2
            result=$($NURIC $1 | \
                ruby -e "require 'json';puts (JSON.pretty_generate JSON.parse(STDIN.read))" \
                2>&1 1>/dev/null)
        else
            message=$3
            result=$($NURIC $2 $1 2>&1 1>/dev/null)
        fi
        if [[ $result != "" ]]; then
            echo -e "$message => $1 ${red}[Failed]${nocolor}"
        else
            echo -e "$message => $1 ${green}[OK]${nocolor}"
        fi
    fi
}

function test_json2nuri {
    if [[ -f $1 && "${1##*.}" = $EXT ]]; then
        json1=$($NURIC $1)
        json2=$($NURIC -m -g $1 | $JSON2NURI -i | $NURIC -i)
        if [[ "$json1" == "$json2" ]]; then
            echo -e "$2 => $1 ${green}[OK]${nocolor}"
        else
            echo -e "$2 => $1 ${red}[Failed]${nocolor}"
        fi
    fi

}

cd $BASEDIR

echo "=== running tests ==="
filelist="$BASEDIR/good-test-files.txt"
for file in $(cat $filelist); do
    if [ ${file:0:1} != "#" ]; then
        # syntax checking
        test $file $OPT_AST "syntax   "

        # type checking
        test $file $OPT_TYPE "typing   "

        # all steps
        test $file "complete "

        # generate JSON and convert JSON to Nuri
#       test_json2nuri $file "json2nuri"
    fi
done
echo "=== done ==="

cd $CURRENTDIR
