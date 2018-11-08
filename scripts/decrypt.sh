#!/bin/bash
if [ -z $AWS_REGION ]; then
    AWS_REGION="us-east-1"
fi

while getopts ":r:" o; do
    case "${o}" in
        r)
            shift
            AWS_REGION=${OPTARG}
            ;;
    esac
    shift
done

if [ -z "$1" ]; then
    if [ -t 0 ]; then
        read -r input
    else
        input=$(cat)
    fi
else
    input=$1
fi

aws kms decrypt \
    --ciphertext-blob fileb://<(echo $input | base64 -D) \
    --output text \
    --region $AWS_REGION \
    --query Plaintext |
    base64 --decode
