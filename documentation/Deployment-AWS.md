# Deploy the SIDAN Plutus Server with AWS Amazon Machine Image (AMI)

Following the [deployment documentation](./Deployment.md), at least an hour is needed for entering `plutus-apps` nix-shell and compiling your project. Thus, a quick AMI image is created specifically for the same `cabal.project` config as the example.

If you are using the exact checkout tag as the AMI, this public AMI could help you save some time.

Using this guide, you still need to take care of the Devops needed in AWS, such as exposing Custom TCP of port 8080 etc.

## Restoring

- Searching `sidan-plutus-server-97b4c1d` when creating EC2 instances. It is available in

* ap-east-1: Asia Pacific (Hong Kong)
* eu-central-1: Europe (Frankfurt)
* us-east-1: US East (N. Virginia)

> Note: Please set it up using your own ssh key

- Connecting to the server as the root user

```
ssh -i "testbuild.pem" ubuntu@ec2-xx-xxx-xx-xxx.us-west-2.compute.amazonaws.com
```

## Using the image

The pre-compiled image is stored in the user `sidan`. So, please start the server under the user `sidan`.

1. Changing user

```
su sidan
```

> Note: The default password for `sidan` is `whatever`

2. Entering `nix-shell`

```
cd ~/sidan-plutus-server/plutus-apps
```

```
. /home/sidan/.nix-profile/etc/profile.d/nix.sh
```

```
nix-shell
```

3. Cloning your own project, we use the example repository as example

```
cd ~/sidan-plutus-server
git clone https://github.com/SIDANWhatever/plutus-cborhex-automation.git
```

4. Copying the built packages to your project directory, this might take a while

```
sudo cp -r plutus-template/dist-newstyle/ plutus-cborhex-automation/example
```

5. Start your project! Our example has an executable defined to start the server, so we do

```
cd plutus-cborhex-automation/example
cabal run sidan-plutus-server &
```

> Note: Adding the `&` at the end to make the server run in the background
