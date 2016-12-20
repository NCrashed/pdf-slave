sudo cp $(which pdf-slave) artifacts
docker build -t ncrashed/pdf-slave -f Dockerfile .
