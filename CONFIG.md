# Configuration examples

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
