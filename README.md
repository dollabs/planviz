# planviz

Planning Network Visualization

As this is a pre-release version **planviz**. You can clone
it (and the required libraries) to build it locally.

Check out the [CHANGELOG](CHANGELOG.md)


## News

The [DOLL](http://dollabs.com/) team (including
[Paul Robertson](https://twitter.com/DrPaulRobertson),
[Dan Cerys](https://twitter.com/dcerys),
[Prakash Manghwani](https://twitter.com/manghwani), and
[Tom Marble](https://twitter.com/tmarble)) recently demoed
PLANVIZ in the talk **Model based programming in PAMELA** at
[Clojure/west](http://clojurewest.org/speakers#tmarble).
Check out the [ClojureTV](https://youtu.be/WLovW6hlYHM) video
and the [slides](https://github.com/dollabs/pamela/blob/master/doc/slides/ClojureWestHelloPamela.pdf).

For more details visit the [PAMELA project page](http://dollabs.com/projects/pamela).

## Documentation

The **planviz** application is part of the [PAMELA](https://github.com/dollabs/pamela) suite of tools.

## Building

Here are the steps to build **planviz** locally:

1. Install [RabbitMQ](https://www.rabbitmq.com/) Server
 * For Debian GNU/Linux systems simply do

   `# apt-get install rabbitmq-server`

1. Install [boot](https://github.com/dollabs/plan-schema#building).
   Please note the steps to install **boot.properties** and **profile.boot**
   to your `~/.boot/` directory (so that the cider deftask is available).

   ````
   $ boot --version
   #http://boot-clj.com
   #Wed Apr 13 13:18:30 CDT 2016
   BOOT_CLOJURE_NAME=org.clojure/clojure
   BOOT_CLOJURE_VERSION=1.8.0
   BOOT_VERSION=2.5.5
   $
   ````

2. Clone and install [plan-schema](https://github.com/dollabs/plan-schema)

   ````
   $ mkdir -p ~/src/github/dollabs
   $ cd ~/src/github/dollabs
   $ git clone https://github.com/dollabs/plan-schema
   $ cd plan-schema
   $ boot local
   ````

3. Clone and install [webtasks](https://github.com/dollabs/webtasks)

   ````
   $ cd ..
   $ git clone https://github.com/dollabs/webtasks
   $ cd webtasks
   $ boot local
   ````

4. Clone and install [webkeys](https://github.com/dollabs/webkeys)

   ````
   $ cd ..
   $ git clone https://github.com/dollabs/webkeys
   $ cd webkeys
   $ boot local
   ````

5. Clone and build **planviz** (_NOTE:_ you may see various warnings
like `WARNING: Use of undeclared Var planviz.components/x21694 at line 42`
that do not prevent building)

   ````
   $ cd ..
   $ git clone https://github.com/dollabs/planviz
   $ cd planviz
   $ boot build-jar
   ````

6. Run the demo
  * *NOTE* this config uses the example plans from **plan-schema**
  * For convenience you may add the [planviz/bin](bin) directory to your `PATH`
(or simply refer to the startup script as `./bin/planviz`).
  * Note that the single argument to `planviz FOO` refers to a configuration file
    with details in `config/FOO.edn`.

   ````
   $ planviz demo
   ````

7. After you see `PLANVIZ server ready` you can open a (or several) browser windows to [http://localhost:8080](http://localhost:8080)

   ````
   $ open http://localhost:8080
   ````

## Commands (*a la* IRC)

* `/who` see which clients are connected
* `/whoami` see the host and port of your browser connection (remote id)
* `/msg user ...` private message for user
* `/nick firefox` set nickname for this connection
* `/list` list available plans
* `/show v1.tpn` show a specific plan
* `/next` view the next plan
* `/prev` view the previous plan
* `/manual` manual mode (do not respond to other user updates)
* `/auto` automatic mode (highlight relevent selections from others)
* `/?` help
* `free format text` broadcast message

## Key bindings

* **z** or **=**  (zoom-in)
* **x** or **-** (zoom-out)
* **ArrowRight** (pan-right)
* **ArrowLeft** (pan-left)
* **ArrowUp** (pan-up)
* **ArrowDown** (pan-down)
* **A-ArrowRight** or **C-ArrowRight** (next-plan) *use Alt- or Control- key*
* **A-ArrowLeft** or **C-ArrowLeft** (prev-plan)
* **1** (reset) *zoom out to 100%*
* **p** (list-plans)

## Development status and Contributing

Please see [CONTRIBUTING](CONTRIBUTING.md) for details on
how to make a contribution.

*NOTE* The tests are (obviously) incomplete!


## Copyright and license

Copyright © 2016 Dynamic Object Language Labs Inc.

Licensed under the [Apache License 2.0](http://opensource.org/licenses/Apache-2.0) [LICENSE](LICENSE)
