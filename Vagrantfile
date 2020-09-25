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
  # config.vm.box = "ubuntu/bionic64"
  config.vm.box = "myaylaci/xubuntu2004-desktop"

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
  config.vm.synced_folder ".", "/usr/local/share/pvs"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  config.vm.provider :virtualbox do |vb|
    # Display the VirtualBox GUI when booting the machine
    vb.gui = true
  
    # Customize the amount of memory on the VM:
    vb.memory = "4096"
    vb.customize [ "modifyvm", :id, "--uartmode1", "disconnected" ]
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
    echo "Before update"
    sudo apt-get update
    sudo apt-get install -y virtualbox-guest-utils
    #sudo apt-get install -y ubuntu-desktop
    sudo apt-get install -y unzip
    sudo apt-get install -y git
    VBoxClient --display
    VBoxClient --draganddrop
    VBoxClient --seamless
    VBoxClient --clipboard
    sudo apt-get install -y emacs
    sudo apt-get install -y tk
    # Install PVS
    cp -r /usr/local/share/pvs /home/vagrant/
    (cd /home/vagrant/pvs; ./install-sh)
    export PATH=/home/vagrant/pvs:/home/vagrant/pvs/bin/ix86_64-Linux:$PATH
    echo "export PATH=/home/vagrant/pvs:/home/vagrant/pvs/bin/ix86_64-Linux:$PATH" >> /home/vagrant/.bashrc
    # Install NASA's pvslib
    echo "installing NASA pvslib"
    (cd /home/vagrant/pvs; git clone https://github.com/nasa/pvslib.git nasalib)
    export PVS_LIBRARY_PATH=/home/vagrant/pvs/nasalib
    echo "export PVS_LIBRARY_PATH=/home/vagrant/pvs/nasalib" >> /home/vagrant/.bashrc
    chown -R vagrant:vagrant pvs
  SHELL
end
