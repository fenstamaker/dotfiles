#!/bin/bash
usage="Usage: $(basename "$0")

Helper script when using AWS KMS to encrypt.

Options:
    -h  show this help text
    -r  Specify AWS Region to run in. (Default is us-east-1 or whatever is in $AWS_REGION)
    -k  Specify the Key Alias."


if [ -z $AWS_REGION ]; then
    AWS_REGION="us-east-1"
fi

while getopts ":r:k:h" opt; do
    case "${opt}" in
        r)
            AWS_REGION=${OPTARG}
            ;;
        k)
            k=${OPTARG}
            ;;
    esac
done
shift $((OPTIND -1))

key_region=$(echo $AWS_REGION | tr '[:lower:]' '[:upper:]' | tr "-" "_")
key_name=$(echo $k | tr '[:lower:]' '[:upper:]' | tr "-" "_")
key_path="${key_region}_${key_name}"

source ~/.envs/.kms || exit 1

key=${!key_path}
if [ -z key ]; then
    key="alias/${k}"
fi

if [ -z "$1" ]; then
    if [ -t 0 ]; then
        read -r input
    else
        input=$(cat)
    fi
else
    input=$1
fi

aws kms encrypt \
    --key-id ${key} \
    --plaintext fileb://<(echo $input) \
    --output text \
    --region $AWS_REGION \
    --query CiphertextBlob
