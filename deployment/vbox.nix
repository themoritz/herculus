{
  machine = { resources, ... }: {
    deployment.targetEnv = "virtualbox";
    deployment.virtualbox = {
      memorySize = 1024;
      headless = true;
    };
  };
}
