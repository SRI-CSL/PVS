# -*- mode: ruby -*-
# vi: set ft=ruby :

# Use vagrant help for general vagrant help.
# For a complete vagrant reference, please see the online documentation at
# https://docs.vagrantup.com.

Vagrant.configure("2") do |config|

  config.ssh.forward_agent = true
  
  config.vagrant.plugins = "vagrant-vbguest"

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://vagrantcloud.com/search.
  config.vm.box = "myaylaci/xubuntu2004-desktop"
  config.vm.synced_folder ".", "/home/vagrant/synced"
  
  config.vm.provider :virtualbox do |vb|
    # Display the VirtualBox GUI when booting the machine
    vb.name = "PVS-vm"
    vb.gui = true
  
    # Customize the amount of memory on the VM:
    vb.memory = "4096"
    vb.cpus = 2
    vb.customize [ "modifyvm", :id, "--uartmode1", "disconnected", "--vram", "128",
                 "--graphicscontroller", "vmsvga" ]
  end

  # This has two parts; the first is in root mode, between ROOT_SHELL occurrences
  # the second is in user mode, between USER_SHELL occurrences.
  # It would probably be nice to refine this to the individual tools; may do that in the future.
  config.vm.provision "shell", inline: <<-ROOT_SHELL
    export DEBIAN_FRONTEND=noninteractive
    echo "apt update"
    sudo apt-get update
    # sudo apt-get dist-upgrade
    echo "Installing unzip, git, emacs, and tk"
    sudo apt-get install -y unzip git emacs tk
    VBoxClient --vmsvga
    VBoxClient --draganddrop
    VBoxClient --seamless
    VBoxClient --clipboard

    # Install Visual Studio Code
    echo "installing Visual Studio Code"
    wget -q https://packages.microsoft.com/keys/microsoft.asc -O- | sudo apt-key add -
    sudo add-apt-repository "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main"
    sudo apt-get install -y code

    # Script to support PVS Emacs Icon
    cat > /usr/local/bin/pvs-script <<PVS_SCRIPT
#!/bin/bash
if [ -d ~/pvs-7.1.0 ]
  then ~/pvs-7.1.0/pvs
  else echo "PVS: you need to click on the Visual Studio Code icon first,\nwhich will prompt you to pick a location (choose the default: /home/vagrant)." | xmessage -nearmouse -file -
fi
PVS_SCRIPT
    chmod a+x /usr/local/bin/pvs-script

    # Install
    echo "installing VScode"
    sudo apt-get install -y software-properties-common apt-transport-https wget
    wget -q https://packages.microsoft.com/keys/microsoft.asc -O- | sudo apt-key add -
    sudo add-apt-repository "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main"
    sudo apt-get install -y code
             
ROOT_SHELL

  # Commands as user
  config.vm.provision "user", type: "shell", privileged: false, inline: <<-USER_SHELL

    cat > ~/.pvs.lisp <<PVS_PATCH
(in-package :pvs-xml-rpc)

(defun xmlrpc-pvs-request (json-request client-url)
  (handler-case
      (let* ((json:*json-identifier-name-to-lisp* #'identity)
	     (req-str (bytestring-to-string json-request))
	     (request (json:decode-json-from-string req-str))
	     (id (cdr (assoc :id request :test #'string-equal)))
	     (method (cdr (assoc :method request :test #'string-equal)))
	     (params (cdr (assoc :params request :test #'string-equal)))
	     (*print-pretty* nil))
	(let* ((result (pvs-json:process-json-request method params id client-url))
	       (jresult (xmlrpc-result result id)))
	  jresult))
    (error (c) (xmlrpc-error (format nil "~a" c)))))

(defun bytestring-to-string (str)
  (let ((octets (map '(simple-array (unsigned-byte 8)) #'char-code str)))
    (excl:octets-to-string octets)))
PVS_PATCH

    # Install NASA's vscode-pvs
    echo "Installing vscode-pvs"
    if [ ! -f /home/vagrant/vscode-pvs ]
    then (cd /home/vagrant; git clone https://github.com/nasa/vscode-pvs.git)
         code --install-extension /home/vagrant/vscode-pvs/releases/vscode-pvs*.vsix
    fi

    echo "Setting desktop icons"
    mkdir -p $(xdg-user-dir DESKTOP)
    cp /usr/share/applications/code.desktop $(xdg-user-dir DESKTOP)/
    chmod a+x $(xdg-user-dir DESKTOP)/code.desktop
    gio set $(xdg-user-dir DESKTOP)/code.desktop metadata::trusted true

    mkdir -p ~/.local/share/applications
    cat > ~/.local/share/applications/pvs.desktop <<PVS_DESKTOP
[Desktop Entry]
Name=PVS
GenericName=Verification System
Exec=pvs-script
Icon=emacs
Type=Application
Terminal=false
PVS_DESKTOP
    chmod a+x ~/.local/share/applications/pvs.desktop
    ln -s ~/.local/share/applications/pvs.desktop $(xdg-user-dir DESKTOP)
    gio set $(xdg-user-dir DESKTOP)/pvs.desktop metadata::trusted true
  USER_SHELL
end
