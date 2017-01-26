let
  region = "us-east-1";
  accessKeyId = "moritz";
in
{
  machine = { resources, ... }: {
    deployment.targetEnv = "ec2";
    deployment.ec2 = {
      accessKeyId = accessKeyId;
      region = region;
      zone = "us-east-1a"; # Same as EBS volume
      instanceType = "t2.micro";
      keyPair = resources.ec2KeyPairs.landingKeys;
      # /dev/xvd[a-e] must be ephemeral devices.
      blockDeviceMapping."/dev/xvdf".disk = "vol-a8925d0e";
    };
  };
  resources.ec2KeyPairs.landingKeys = {
    inherit region accessKeyId;
  };
}
