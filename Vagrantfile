# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.

Vagrant.configure("2") do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.

  # Owre - tried all the following, didn't care for them.
  # config.vm.box = "myaylaci/xubuntu2004-desktop" # Comes up with error, flakey
  # config.vm.boot_timeout = 600
  # config.vm.box = "ubuntu/bionic64"
  # config.vm.box = "hashicorp/bionic64"
  # config.vm.box = "bento/ubuntu-20.04" # No X11
  # config.vm.box = "box-cutter/ubuntu1604-desktop" # could not resolve host
  
  config.vm.box = "bstoots/xubuntu-16.04-desktop-amd64"
  
  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # Making the current directory (e.g., pvs-7.1) synced with .pvs_shared for copying
  config.vm.synced_folder ".", "/home/vagrant/.pvs_shared"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  config.vm.provider :virtualbox do |vb|
    # Display the VirtualBox GUI when booting the machine
    vb.name = "pvs-vm"
    vb.gui = true
  
    # Customize the amount of memory on the VM:
    vb.memory = "4096"
    vb.customize [ "modifyvm", :id, "--uartmode1", "disconnected", "--vram", "128",
                 "--graphicscontroller", "vmsvga" ]
  end
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push strategies
  # such as FTP and Heroku are also available. See the documentation at
  # https://docs.vagrantup.com/v2/push/atlas.html for more information.
  # config.push.define "atlas" do |push|
  #   push.app = "YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.
  config.vm.provision "shell", inline: <<-SHELL
    echo "apt update"
    sudo apt-get update
    echo "Installing unzip, git, emacs, and tk"
    sudo apt-get install -y unzip
    sudo apt-get install -y git
    sudo apt-get install -y emacs
    sudo apt-get install -y tk
    VBoxClient --vmsvga
    VBoxClient --draganddrop
    VBoxClient --seamless
    VBoxClient --clipboard
    # Install PVS
    echo "Installing PVS"
    cp -r /home/vagrant/.pvs_shared /home/vagrant/pvs
    chown -R vagrant:vagrant /home/vagrant/pvs
    (cd /home/vagrant/pvs; sh install-sh)
    export PATH=/home/vagrant/pvs:/home/vagrant/pvs/bin/ix86_64-Linux:$PATH
    echo "export PATH=/home/vagrant/pvs:/home/vagrant/pvs/bin/ix86_64-Linux:$PATH" >> /home/vagrant/.bashrc
    # Install NASA's pvslib
    echo "Installing NASA pvslib"
    [ -d /home/vagrant/pvs/pvslib ] || (cd /home/vagrant/pvs; git clone https://github.com/nasa/pvslib.git)
    export PVS_LIBRARY_PATH=/home/vagrant/pvs/pvslib
    echo "export PVS_LIBRARY_PATH=/home/vagrant/pvs/pvslib" >> /home/vagrant/.bashrc
    # Install vscode-pvs
    echo "Installing vscode-pvs"
    [ -d /home/vagrant/pvs/vscode-pvs ] || (cd /home/vagrant; git clone https://github.com/nasa/vscode-pvs.git)
    # Install NASA's vscode-pvs
    echo "installing vscode"
    sudo apt-get install -y software-properties-common apt-transport-https wget
    wget -q https://packages.microsoft.com/keys/microsoft.asc -O- | sudo apt-key add -
    sudo add-apt-repository "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main"
    sudo apt-get install -y code
    # shutdown -r now #restart
  SHELL
end
