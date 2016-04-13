# planviz

Planning Network Visualization

As this is a pre-release version **planviz**. You can clone
it (and the required libraries) to build it locally.

Check out the [CHANGELOG](CHANGELOG.md)

## Documentation

The **planviz** application is part of the [PAMELA](https://github.com/dollabs/pamela) suite of tools.

## Building

Here are the steps to build **planviz** locally:

1. Install [RabbitMQ](https://www.rabbitmq.com/)
 * For Debian GNU/Linux systems simply do

   `# apt-get install rabbitmq-server`

1. Install [boot](https://github.com/dollabs/plan-schema#building)

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

5. Clone this repo

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

   ````
   $ planviz demo
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
