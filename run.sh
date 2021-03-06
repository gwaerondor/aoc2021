#!/bin/bash

ACTION="both"
DAY="1"

GO=go
[ -f "/usr/local/go/bin/go" ] && GO="/usr/local/go/bin/go"

main() {
    if [ "${*}" = "test all" ]; then
        ${GO} test ./...
        exit ${?}
    fi
    [ ! -z "${1}" ] && DAY="$(echo "${1}" | sed 's/^\([0-9]\)$/0\1/')"
    [ ! -z "${2}" ] && ACTION="${2}"

    local PROBLEM_PATH
    PROBLEM_PATH="problems/day${DAY}/"
    local CMD
    if [ "${ACTION}" = "both" ]; then
        CMD="${GO} test && ${GO} run Day${DAY}.go"
    elif [ "${ACTION}" = "test" ]; then
        CMD="${GO} test"
    elif [ "${ACTION}" = "run" ]; then
        CMD="${GO} run Day${DAY}.go"
    fi
    (cd "${PROBLEM_PATH}"
     bash -c "${CMD}"
    )
}

main $@
