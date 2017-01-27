{
  machine = { resources, ... }: {
    deployment.targetEnv = "virtualbox";
    deployment.virtualbox = {
      headless = true;
      memorySize = 1024;
      disks.xvdf = {
        port = 1;
        size = 1024;
      };
    };
  };
}
