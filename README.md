# planviz

Planning Network Visualization

As this is a pre-release version **planviz**. You can clone
it (and the required libraries) to build it locally.

Check out the [CHANGELOG](CHANGELOG.md)

## Documentation

The **planviz** application is part of the [PAMELA](https://github.com/dollabs/pamela) suite of tools.

## Building

The **planviz** application uses [boot](http://boot-clj.com/) as a build tool.

Check [here for more information on setting up boot](https://github.com/dollabs/plan-schema#building).

Here are the steps to build **webtasks** locally:
1. Install [RabbitMQ](https://www.rabbitmq.com/)
 * For Debian GNU/Linux systems simply do `apt-get install rabbitmq-server`
1. Install boot (see links above)
2. Clone and install [plan-schema](https://github.com/dollabs/plan-schema)
3. Clone and install [webtasks](https://github.com/dollabs/webtasks)
4. Clone and install [webkeys](https://github.com/dollabs/webkeys)
5. Clone this repo
  * `boot build-jar`
  * For convenience you may add the [planviz/bin](bin) directory to your `PATH`
(or simply refer to the startup script as `./bin/planviz`).
  * `boot seattle` (this config uses the example plans from **plan-schema**)


## Invocation Example

````
planviz -v -v -i ../viz/common/choice-tpn.json -i ../plan-schema/html/examples/sept14/sept14.htn.json=../plan-schema/html/examples/sept14/sept14.tpn.json &
open localhost:8080
````

## Quick configuration

It is possible to configure PLANVIZ to use a configuration file instead
of constructing a long command line. Here's an example of creating
a config file and running with just **planviz demo**:


````
tmarble@cerise 270 :) planviz -v -v --host localhost -i ../plan-schema/html/examples/sept14/sept14.htn.json=../plan-schema/html/examples/sept14/sept14.tpn.json visualize save-config
saved configuration to /home/tmarble/src/lispmachine/pamela/planviz/config/planviz.edn
tmarble@cerise 271 :) mv config/planviz.edn config/demo.edn
tmarble@cerise 272 :) cat config/demo.edn
{:verbose 2,
 :exchange "tpn-updates",
 :input
 ["../plan-schema/html/examples/sept14/sept14.htn.json=../plan-schema/html/examples/sept14/sept14.tpn.json"],
 :rmq-host "localhost",
 :rmq-port 5672,
 :host "localhost",
 :port 8080,
 :arguments ["visualize"]}
tmarble@cerise 273 :) planviz demo
repl?: false
cwd: /home/tmarble/src/lispmachine/pamela/planviz
verbosity level: 2
rmq-host: localhost
rmq-port: 5672
exchange: tpn-updates
host: localhost
port: 8080
input: [../plan-schema/html/examples/sept14/sept14.htn.json=../plan-schema/html/examples/sept14/sept14.tpn.json]
cmd: visualize (valid)
Sifting output files...
Compiling ClojureScript...
• public/js/app.js
LOAD-INPUT: ../plan-schema/html/examples/sept14/sept14.htn.json=../plan-schema/html/examples/sept14/sept14.tpn.json
PLANVIZ server ready
...Writing target dir(s)...
````


## Development status and Contributing

Please see [CONTRIBUTING](CONTRIBUTING.md) for details on
how to make a contribution.

*NOTE* The tests are (obviously) incomplete!


## Copyright and license

Copyright © 2016 Dynamic Object Language Labs Inc.

Licensed under the [Apache License 2.0](http://opensource.org/licenses/Apache-2.0) [LICENSE](LICENSE)
