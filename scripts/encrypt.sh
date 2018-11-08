#!/bin/bash
usage="$(basename "$0") [-r] [-k n] -- program to calculate the answer to life, the universe and everything

where:
    -h  show this help text
    -s  set the seed value (default: 42)"


if [ -z $AWS_REGION ]; then
    AWS_REGION="us-east-1"
fi

while getopts ":r:k:" o; do
    case "${o}" in
        r)
            shift
            AWS_REGION=${OPTARG}
            ;;
        k)
            shift
            k=${OPTARG}
            ;;
    esac
    shift
done

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
