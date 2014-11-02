#!/bin/bash

BASEDIR="$(dirname $0)"
CURRENTDIR=$(pwd)
OPT_TYPE="-t"
#OPT_YAML="-yaml"
OPT_AST="-x"
#OPT_AST_JSON="-ast-json"
BIN="$BASEDIR/../ocaml/nuric"
EXT="nuri"

red='\x1B[0;31m'
green='\x1B[0;32m'
nocolor='\x1B[0m'

function test {
	if [[ -f $1 && "${1##*.}" = $EXT ]]; then
		if [ $# == 1 ]; then
			result=$($BIN $1 | \
				ruby -e "require 'json';puts (JSON.pretty_generate JSON.parse(STDIN.read))" \
				2>&1 1>/dev/null)
		else
			result=$($BIN $2 $1 2>&1 1>/dev/null)
		fi
		if [[ $result != "" ]]; then
			echo -e "$2 => $1 ${red}[Failed]${nocolor}"
		else
			echo -e "$2 => $1 ${green}[OK]${nocolor}"
		fi
	fi
}

function test_ast_json {
	if [[ -f $1 && "${1##*.}" == $EXT ]]; then
		result=$($BIN $OPT_AST_JSON $1 | \
			ruby -e "require 'json'; puts (JSON.pretty_generate JSON.parse(STDIN.read))" \
			2>&1 1>/dev/null)
		if [[ $result != "" ]]; then
			echo -e "$OPT_AST_JSON => $1 ${red}[Failed]${nocolor}"
		else
			echo -e "$OPT_AST_JSON => $1 ${green}[OK]${nocolor}"
		fi
	fi
}

cd $BASEDIR

echo "=== running tests ==="
filelist="$BASEDIR/good-test-files.txt"
for file in $(cat $filelist); do
	if [ ${file:0:1} != "#" ]; then
		# AST: -ast
		test $file $OPT_AST
		# AST: -ast-json
		#test_ast_json $file
		# type: -type
		test $file $OPT_TYPE
		# JSON: -json
		test $file
		# YAML: -yaml
		#test $file $OPT_YAML
	fi
done
echo "=== done ==="

cd $CURRENTDIR
