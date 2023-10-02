#!/bin/bash

# Define the Docker image name and Dockerfile location
IMAGE_NAME="lil_jit"
DOCKERFILE_PATH="./Dockerfile"  # Make sure this points to your actual Dockerfile

# Capture all arguments to pass to the application inside the Docker container
APP_ARGS="$@"

# Check if the Docker image exists
docker image inspect $IMAGE_NAME > /dev/null 2>&1

# $? is a special variable that holds the exit code of the last command executed
if [ $? -eq 0 ]; then
    echo "Image exists, running the container..."
else
    echo "Image does not exist, building it first..."
    docker build -t $IMAGE_NAME -f $DOCKERFILE_PATH . || { echo "Docker build failed"; exit 1; }
fi

# Run the Docker container and pass the arguments to the application inside it
docker run --rm --name lil_jit $IMAGE_NAME $APP_ARGS || { echo "Docker run failed"; exit 1; }
