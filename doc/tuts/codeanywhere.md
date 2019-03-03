# Setting up Codeanywhere

##### 1. Upgrade a your linux distribution
```sh
~/workspace$ sudo apt-get update
~/workspace$ sudo apt-get upgrade
```

##### 2. Install Lua, Python, and Create aliases
```sh
~/workspace$ sudo apt-get install lua5.3 luajit
~/workspace$ sudo apt-get install python3 python3-pip python3-setuptools
```

Next, open your `~/.bashrc` and add the following two lines to it:

```sh
alias lua='/usr/bin/lua5.3'
alias python='/usr/bin/python3'
alias pip='/usr/bin/pip3'
```

Save, and close the `.bashrc` file. Now, to update your environment, run the following command

```sh
~/workspace$ source ~/.bashrc
```

##### 3. Install Pycco

We'll install this from source. Make a temporary empty director in `~/workspace`

```sh
~/workspace$ cd ~
~/workspace$ mkdir installs && cd installs 
~/workspace$ git clone git://github.com/pycco-docs/pycco.git
~/workspace$ cd pycco
~/workspace$ sudo python3 setup.py install
~/workspace$ cd ~
```
##### 4. Obtain the project source

```sh 
~/workspace$ git clone https://github.com/d-u-o/101 ./duo
~/workspace$ cd duo
~/workspace$ cd etc
~/workspace$ .ide
~/workspace$ ../src
~/workspace$ make eg1
```

