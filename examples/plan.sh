#!/bin/bash

../ocaml/nuric -p $@ > plan.json && ../utils/nuri-graph-plan --png plan.json && cat plan.json | ../utils/prettyjson
