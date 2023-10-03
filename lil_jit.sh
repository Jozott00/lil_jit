#!/bin/bash

# Define the Docker image name and Dockerfile location
IMAGE_NAME="lil_jit"
DOCKERFILE_PATH="./Dockerfile"  # Make sure this points to your actual Dockerfile

# Capture all arguments to pass to the application inside the Docker container
APP_ARGS="$@"

# Extract the last argument (assumed to be the file path)
FILE_PATH="${!#}"

# Initialize an empty string for the volume mount option
VOLUME_OPTION=""

# Check if the last argument is a valid file path and not a flag
if [[ -f $FILE_PATH && ! $FILE_PATH =~ ^- ]]; then
    VOLUME_OPTION="-v $(pwd)/$FILE_PATH:/app/$FILE_PATH"
elif [[ ! -f $FILE_PATH && ! $FILE_PATH =~ ^- ]]; then
    echo "File not found: $FILE_PATH"
    exit 1
fi

# Check if the Docker image exists
docker image inspect $IMAGE_NAME > /dev/null 2>&1

# $? is a special variable that holds the exit code of the last command executed
if [ $? -ne 0 ]; then
    echo "Image does not exist, building it first..."
    docker build -t $IMAGE_NAME -f $DOCKERFILE_PATH . || { echo "Docker build failed"; exit 1; }
fi

# Run the Docker container, conditionally mount the file, and pass the arguments to the application inside it
docker run --rm --platform linux/arm64 --name lil_jit $VOLUME_OPTION $IMAGE_NAME $APP_ARGS || { echo "Docker run failed"; exit 1; }
