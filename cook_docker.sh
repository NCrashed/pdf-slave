docker build -t pdf-slave-build -f Dockerfile_build .
docker run --rm -t -v $(pwd)/artifacts:/data/artifacts pdf-slave-build
docker build -t pdf-slave -f Dockerfile .
