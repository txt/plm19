# Setting up Codeanywhere

#### 1. Upgrade a your linux distribution
```sh
~/workspace$ sudo apt-get update
~/workspace$ sudo apt-get upgrade
```

#### 2. Install Lua, Python, and Create aliases
```sh
~/workspace$ sudo apt-get install -y lua5.3 luajit
~/workspace$ sudo apt-get install - y python3 python3-pip python3-setuptools
```
Let's create some aliases. Open your `~/.bashrc` and (optionally, at around line 94) add the following two lines to it:

```sh
alias lua='/usr/bin/lua5.3'
alias python='/usr/bin/python3'
alias pip='/usr/bin/pip3'
```

Save, and close the `.bashrc` file. Now, to update your environment, run the following command

```sh
~/workspace$ source ~/.bashrc
```

#### 3. Install Pycco

We'll install this from source. Make a temporary empty director in `~/workspace`

```sh
~/workspace$ cd ~
~/workspace$ mkdir installs && cd installs 
~/workspace$ git clone git://github.com/pycco-docs/pycco.git
~/workspace$ cd pycco
~/workspace$ sudo python3 setup.py install
~/workspace$ cd ~
```
#### 4. Obtain the project source

```sh 
~/workspace$ git clone https://github.com/d-u-o/101 ./duo
~/workspace$ cd duo
~/workspace$ cd etc
~/workspace$ .ide
~/workspace$ ../src
~/workspace$ make eg1
```

This should produce

```
### eg1 ######################

outlook,$temp,wind,!play,>dom
sunny,85,FALSE,no,0
sunny,80,TRUE,no,0
overcast,83,FALSE,yes,0
rainy,70,FALSE,yes,0
rainy,68,FALSE,yes,0
rainy,65,TRUE,no,0
overcast,64,TRUE,yes,0
sunny,72,FALSE,no,0
sunny,69,FALSE,yes,0
rainy,75,FALSE,yes,0
sunny,75,TRUE,yes,0
overcast,72,TRUE,yes,0
overcast,81,FALSE,yes,0
rainy,71,TRUE,no,0
Duo101> cabox/duo/src 16>
```

