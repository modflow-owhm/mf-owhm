#!/bin/bash

CWD=$(pwd)

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

echo; echo; echo; echo "../../bin/zonebudget.nix  <Zone_CMD_Answers.txt"; echo; echo
../../bin/zonebudget.nix  <Zone_CMD_Answers.txt

echo; echo; echo; echo "../../bin/zonebudget.nix  <Zone_CMD_Answers_INTERNAL1.txt"; echo; echo
../../bin/zonebudget.nix  <Zone_CMD_Answers_INTERNAL1.txt
echo; echo; echo; echo "../../bin/zonebudget.nix  <Zone_CMD_Answers_INTERNAL2.txt"; echo; echo
../../bin/zonebudget.nix  <Zone_CMD_Answers_INTERNAL2.txt

echo; echo; echo; echo "../../bin/zonebudget.nix  <Zone_CMD_Answers_EXTERNAL1.txt"; echo; echo
../../bin/zonebudget.nix  <Zone_CMD_Answers_EXTERNAL1.txt
echo; echo; echo; echo "../../bin/zonebudget.nix  <Zone_CMD_Answers_EXTERNAL2.txt"; echo; echo
../../bin/zonebudget.nix  <Zone_CMD_Answers_EXTERNAL2.txt

echo; echo; echo; echo "../../bin/zonebudget.nix  <Zone_CMD_Answers_OPENCLOSE1.txt"; echo; echo
../../bin/zonebudget.nix  <Zone_CMD_Answers_OPENCLOSE1.txt
echo; echo; echo; echo "../../bin/zonebudget.nix  <Zone_CMD_Answers_OPENCLOSE2.txt"; echo; echo
../../bin/zonebudget.nix  <Zone_CMD_Answers_OPENCLOSE2.txt

echo; echo; echo

cd "$CWD"

read -p "Press [Enter] to end script  "



