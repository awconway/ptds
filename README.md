# Validating the perioperative thirst discomfort scale for measuring thirst discomfort prior to procedures

<!-- badges: start -->
<!-- badges: end -->

>This repository hosts raw data and code required to completely reproduce the statistical analyses.  All code is in `R`. The drake package was used to manage the workflow.

## Reproducible analysis with Docker

The statistical anlyses requires various packages to be installed, and may not work properly if package versions have changed. Therefore, a [Docker image is provided](https://hub.docker.com/repository/docker/awconway/ptds) to run the code reproducibly.

### Run Docker locally

**If you already have [docker](https://docs.docker.com/install/) installed**

- Run the following in a terminal (substituting in a user name and password):

```
docker run -d -p 8787:8787 -e USER=<user> -e PASSWORD=<password> awconway/ptds
```

- Open a web browser and go to: localhost:8787
- Enter your username and password to enter an RStudio session.
- Create a new project from version control (File > New project > Version Control > Git > https://github.com/awconway/ptds.git )
- Run this line of code in the console to reproduce the analysis:

```
drake::r_make()
```

You will see the targets being built by `drake`, and the final manuscript should be compiled at the end as `index.docx` in the `manuscript` directory.


### Run Docker on a Cloud

Instead of installing docker on your system you can run it on a remote server, such as [Digital Ocean](https://www.digitialocean.com). This [link](https://m.do.co/c/89cf8df06791) provides you with $100 free credit to use for a 60-day period. After signing up, follow these steps to run this project on a Digital Ocean droplet:

- Create a DigitalOcean droplet. Choose a server with Docker installed from the *Marketplace* menu and choose a size for your server (number of CPUs and amount of RAM).

- Select `User data` from the `Select additional options` section and enter the text as displayed below (substituting in a username and password).

```
#cloud-config
runcmd:
  - docker run -d -p 8787:8787 -e USER=<user> -e PASSWORD=<password> awconway/ptds
```

- Create the droplet.

- Wait a few minutes for the docker image to load into the server then open a web browser and type in the ip address of the droplet you just created followed by the port 8787 (e.g. ipaddress:8787).
- Enter your username and password to enter an RStudio session.
- Create a new project from version control (File > New project > Version Control > Git > https://github.com/awconway/ptds.git )
- Run this line of code in the console to reproduce the analysis:

```
drake::r_make()
```

You will see the targets being built by `drake`, and the final manuscript should be compiled at the end as `index.docx` in the `manuscript` folder.


- *Destroy the DigitalOcean droplet when finished inspecting the analyses.*

