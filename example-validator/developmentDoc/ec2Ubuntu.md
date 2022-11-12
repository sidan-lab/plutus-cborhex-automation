# Development Documentation on Local VM Testing

- Environment: AWS EC2
- Date Nov 12 2022

## Setup

1. Server Config

```
Ubuntu Server 22.04 LTS (HVM), SSD Volume Type
Architecture 64-bit (x86)

t3.large
Family: t3   2 vCPU   8GiB Memory
On-Demand Linux pricing: 0.0832 USD per Hour
On-Demand Windows pricing: 0.1108USD per Hour

Volume 1 (AMI Root) (Custom) 50 GiB gp2

Create SSH key
```

2. Enter into VM

3. Add user, make it `sudo`, change user to newly created one

```
sudo adduser whatever
```

> - after typing this command following prompts would up
>
> 1. passward
> 2. repeat password
> 3. Full name
> 4. Room number
> 5. Work phone
> 6. Home phone
> 7. Other
> 8. Is the info correct

```
sudo gpasswd -a whatever sudo
```

```
su whatever
```

> - have to type password here

## Setup

Setup guide taking reference to [plutus community doc](https://plutus-community.readthedocs.io/en/latest/#Environment/Build/Ubuntufresh/)

1. Update the package and install `curl`

```
sudo sh -c 'apt update && apt install curl'
```

2. Install `git`

```
sudo apt-get install git
```

3. Install `nix`

```
sh <(curl -L https://nixos.org/nix/install) --no-daemon
```

4. Set the environment

```
. /home/whatever/.nix-profile/etc/profile.d/nix.sh
```

5. Adding IOHK caches

```
mkdir -p ~/.config/nix
echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> ~/.config/nix/nix.conf
sudo echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> ~/.config/nix/nix.conf
sudo echo 'extra-experimental-features = flakes' >> ~/.config/nix/nix.conf
```

## Installing the project

1. Create a separate directory

```
mkdir ~/validator-endpoint
```

2. Clone the `plutus-apps`

```
cd ~/validator-endpoint/
git clone https://github.com/input-output-hk/plutus-apps.git
```

3. Clone the Cardano project, here we assume it is open source

```
git clone https://github.com/SIDANWhatever/plutus-cborhex-automation.git
```

4. Entering nix-shell with IOHK binaries, here we checkout latest tag `7b4c1da03faf9bc35f348802fb7927231657e75`

```
cd ~/validator-endpoint/plutus-apps
git checkout 7b4c1da03faf9bc35f348802fb7927231657e75
nix-shell
```

## Starting the endpoints

- Prior to anything started, logout the terminal, and login directly as the user where the nix-shell is installed

```
exit
exit
exit
```

```
cd ~/.ssh
ssh -i "testbuild.pem" whatever@ec2-xx-xxx-xx-xxx.us-west-2.compute.amazonaws.com
```

```
cd ~/plutus-cborhex-automation/example-validator
```

ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDaWIB3gDSs0cERhc2k8s9HTTHupN7mBQ9QQvjZnKOzNkXuejaiVzMCNC9qf3z8AguCEC2ZZTjExJ6G038sUEu2nRToqddKfC20SKsBpF7mYjArdBwqE0LMMzQgvkveJdUO/lZOAbjQt7KPWii6oiUjntLdYxuLkk+2twsG8hTEAUWLyhZolE+wb>
