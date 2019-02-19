#!/bin/bash
if [ -z $AWS_REGION ]; then
    AWS_REGION="us-east-1"
fi

while getopts ":r:" o; do
    case "${o}" in
        r)
            AWS_REGION=${OPTARG}
            ;;
    esac
done
shift $((OPTIND -1))

if [ -z "$1" ]; then
    input=$(cat)
else
    input=$1
fi

echo ""
regex='^(.*)\%\{(.*)\}(.*)$';
if [[ $input =~ $regex ]]; then
    while [[ $input =~ $regex ]]; do
        pre="${BASH_REMATCH[1]}"
        body="${BASH_REMATCH[2]}"
        post="${BASH_REMATCH[3]}"

        result=$(aws kms decrypt \
                     --ciphertext-blob fileb://<(echo $body | tr -d "[[:space:]]" | base64 --decode) \
                     --output text \
                     --region $AWS_REGION \
                     --query Plaintext |
                     base64 --decode)

        input="$pre$result$post"
    done
    echo "$input";
else
    body=$input
    result=$(aws kms decrypt \
                 --ciphertext-blob fileb://<(echo $body | tr -d "[[:space:]]" | base64 --decode) \
                 --output text \
                 --region $AWS_REGION \
                 --query Plaintext |
                 base64 --decode)
    echo $result
fi
