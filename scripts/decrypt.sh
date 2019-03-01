#!/bin/bash
set -e
HELP=$(cat << EOF
Usage: $(basename "$0") (-k <id> | --key <id>) [OPTIONS] [INPUT]
Decrypts secrets with an AWS KMS key.

With no INPUT, or when INPUT is -, read standard input.
EOF
)

HELP_OPTS=$(cat <<EOF
  -h, --help \t Shows this help text.
  -r <region>, --region <region> \t The region to use. Overrides config/env settings.
  -v, --verbose \t Prints more logs
EOF
)

usage() {
    echo "$HELP"
    echo ""
    column -t -s $'\t' <(echo -e "$HELP_OPTS")
    exit 1
}

iecho() {
    if [ -t 0 ]; then
        >&2 echo "$1"
    fi
}

debug() {
    if [ $VERBOSE -eq 1 ]; then
        >&2 echo "[DEBUG] $1"
    fi
}

error() {
    >&2 echo "[ERROR] $1"
}

prepare() {
    echo $1 | gtr -d "[[:space:]]" | gbase64 --decode
}

AWS_REGION="us-east-1"
VERBOSE=0

while :; do
    case $1 in
        -h|-\?|--help)
            usage
            ;;

        -r|--region)
            if [ "$2" ]; then
                AWS_REGION=$2
                shift
            else
                error "'$1' requires a non-empty argument."
                echo ""
                usage
            fi
            ;;

        -v|--verbose)
            VERBOSE=1
            ;;

        --)
            shift
            break
            ;;

        -?*)
            error "Unknown option ${1}"
            echo ""
            usage
            ;;

        *)
            break
    esac

    shift

done

debug "USING ${AWS_REGION}"
if [ -z "$1" ] || [ "$1" = "-" ]; then
    debug "No INPUT arg. Reading STDIN"
    iecho "Press ^D twice to stop reading (or once at start of line)"
    input=$(cat)
    iecho "---"
else
    debug "INPUT arg given."
    input=$1
fi

debug "Decrypting input..."
regex='^(.*)\%\{(.*)\}(.*)$';
if [[ $input =~ $regex ]]; then
    while [[ $input =~ $regex ]]; do
        pre="${BASH_REMATCH[1]}"
        body="${BASH_REMATCH[2]}"
        post="${BASH_REMATCH[3]}"

        result=$(aws kms decrypt \
                     --ciphertext-blob fileb://<(prepare $body) \
                     --output text \
                     --region $AWS_REGION \
                     --query Plaintext | gbase64 --decode)

        input="$pre$result$post"
    done
    echo "$input";
else
    body=$input
    result=$(aws kms decrypt \
                 --ciphertext-blob fileb://<(prepare $body) \
                 --output text \
                 --region $AWS_REGION \
                 --query Plaintext | gbase64 --decode)
    echo $result
fi
