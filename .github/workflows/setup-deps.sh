# Echo command line and fail on any error
set -e
set -x

wget --quiet https://github.com/souffle-lang/souffle/releases/download/1.7.1/souffle_1.7.1-1_amd64.deb
sudo apt-get install ./souffle_1.7.1-1_amd64.deb