#!/bin/bash
set -e
HELP=$(cat << EOF
Usage: $(basename "$0") (-k <id> | --key <id>) [OPTIONS] [INPUT]
Encrypts secrets with an AWS KMS key.

With no INPUT, or when INPUT is -, read standard input.
EOF
)
HELP_OPTS=$(cat <<EOF
  -h, --help \t Shows this help text.
  -r <region>, --region <region> \t The region to use. Overrides config/env settings.
  -k <alias>, --key <alias> \t The unique identifier for the encryption key. Can be the alias defined in ~/.envs/.kms, the alias of the actual key, or the UUID for the key.
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

AWS_REGION="us-east-1"
VERBOSE=0
KEY=

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

        -k|--key)
            if [ "$2" ]; then
                KEY=$2
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

debug "USING ${AWS_REGION} ${KEY}"
UUID_REGEX=^\{?[A-F0-9a-f]{8}-[A-F0-9a-f]{4}-[A-F0-9a-f]{4}-[A-F0-9a-f]{4}-[A-F0-9a-f]{12}\}?$
KMS_ALIAS_FILE="${HOME}/.envs/.kms"

if [ -z $KEY ]; then
    error "Missing key argument."
    echo ""
    usage
fi

key=
if [[ $KEY =~ $UUID_REGEX ]]; then
    debug "Provided key is an UUID."
    key=$KEY
else
    if [ -f $KMS_ALIAS_FILE ]; then
        debug "Found KMS alias file. Loading ${KMS_ALIAS_FILE}"
        source $KMS_ALIAS_FILE

        key_region=$(echo $AWS_REGION | gtr '[:lower:]' '[:upper:]' | tr "-" "_")
        key_name=$(echo $KEY | gtr '[:lower:]' '[:upper:]' | tr "-" "_")
        key_path="${key_region}_${key_name}"

        key=${!key_path}
        if [ -z $key ]; then
            debug "Could not find alias in KMS alias file. Falling back to key alias."
            key="alias/${KEY}"
        else
            debug "Found key alias in KMS alias file."
        fi
    else
        debug "No KMS alias file. Defaulting to key alias."
        key="alias/${KEY}"
    fi
fi

if [ -z "$1" ] || [ "$1" = "-" ]; then
    debug "No INPUT arg. Reading STDIN"
    iecho "Press ^D twice to stop reading (or once at start of line)"
    input=$(cat)
    iecho "---"
else
    debug "INPUT arg given."
    input=$1
fi

debug "Encrypting input with ${key}..."
aws kms encrypt \
    --key-id ${key} \
    --plaintext fileb://<(echo $input | gtr -d " \t\n\r") \
    --output text \
    --region $AWS_REGION \
    --query CiphertextBlob
