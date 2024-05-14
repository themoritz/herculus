# Herculus Landing Page

## Developing

```shell
cd landing-page/client
nix-shell --command "jekyll serve --livereload --incremental"
```

Open your browser and go to the printed `Server address`. CSS
and images will be injected when they change, and all pages will automatically
reload when updated.

## Deployment

```shell
nix-shell --command "jekyll build"

scp -r _site user@host:/root/code/herculus/landing-page/client/
```
