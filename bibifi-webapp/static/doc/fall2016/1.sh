curl -sSf https://static.rust-lang.org/rustup.sh | sudo sh
sudo apt-add-repository ppa:tsutsu/elixir
sudo apt-get update
sudo apt-get install -y julia
sudo apt-get install -y elixir
stack setup --resolver lts-7.0
stack install aeson
sudo apt-get install -y clang golang-go cmake libssl-dev zlib1g-dev libcppunit-dev llvm python-demjson python3-crypto python-zmq libzmq-dev libcrypto++-dev libseccomp-dev seccomp libc++-dev libc++abi-dev
stack install network-simple
sudo apt-get install -y libboost-all-dev
sudo apt-get install -y flex bison
sudo apt-get install -y nodejs npm
sudo apt-get install -y default-jdk
sudo apt-get install -y python-pyparsing
sudo apt-get install -y golang
sudo apt-get install -y maven
sudo apt-get install -y libjson-simple-java
sudo apt-get install -y postgresql postgresql-contrib
echo debconf shared/accepted-oracle-license-v1-1 select true | sudo debconf-set-selections
echo debconf shared/accepted-oracle-license-v1-1 seen true | sudo debconf-set-selections
sudo add-apt-repository -y ppa:webupd8team/java; sudo apt-get update; sudo apt-get install -y oracle-java8-installer