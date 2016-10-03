sudo add-apt-repository -y ppa:ubuntu-lxc/lxd-stable; sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
echo "deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.0 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.0.list
sudo apt-get update
sudo apt-get install -y libasio-dev libboost-all-dev python-pip python-dev build-essential mongodb-org python3-pyparsing clang-3.8 python3-pip scala-library scala python-ply socket python-mysqldb mono-complete Python-psycopg2 golang cpanminus
sudo pip install --upgrade pip
sudo pip install --upgrade virtualenv
sudo pip install flake8 nose coverage grako pyrser nose2 setuptools cov-core passlib antlr4-python2-runtime pydblite
sudo mkdir -p /opt/packages/gradle; cd /opt/packages/gradle; sudo wget https://services.gradle.org/distributions/gradle-3.1-bin.zip; sudo unzip gradle-3.1-bin.zip; sudo ln -s /opt/packages/gradle/gradle-3.1/bin/gradle /usr/bin/gradle; cd -
sudo pip3 install networkx pyrser utility pytest twisted simplejson
sudo cpanm install Marpa::R2
sudo pip install asteval SQLAlchemy Cython bcrypt
sudo pip3 install ujson
sudo apt-get install -y fp-compiler bcrypt
stack install network parsec attoparsec lens json