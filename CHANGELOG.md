# Change Log

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

### [Unreleased]

Changed
* _TBD_

### [0.9.1] - 2016-11-29

Changed
- Updated plan-schema to 0.2.13 (fixes TPN selection bug)
- Removed CIDER references from build.boot
- Added boot.properties to ensure BOOT_CLOJURE_VERSION=1.8.0
- Updated dependencies
- Streamlined build.boot (use conventional "resources" instead of "html")
- Updated plan-schema to 0.2.14 to Close #42

### [0.9.0] - 2016-11-16

Added
- No longer change all user's view to the latest selection (Closes #28)
- Resolve #30 (cleanup relaxed tpn values, via plan-schema)
- Resolve event storm bug #33. Also quiets peer browser sessions showing a
  spinner or giving a status message when loading files.
- Resolve Selection bug #35. Also disable ring-default not-modified
  headers at log level :trace and :debug (to force *.cljs reloading)
- Update dependencies
- Added settings dialog menu. Closes:
  * Leverages various webfonts to allow run time customization
  * BB 37 TPNVIZ: create a Settings page
  * BB 48 TPNVIZ: Save settings (server side)
  * #32 Config Option to change HTN node Font and Size
- Performance:
  * use keyword-identical? in favor of = for keyword comparisons
  * use simple keywords as key-fn's where applicable
- NOTE: This version requires [dollabs/webkeys "0.4.0"]

### [0.8.9] - 2016-10-26

Changed
- Resolve #30 (cleanup relaxed tpn values, via plan-schema)

### [0.8.8] - 2016-10-17

Changed
- Leverage plan-schema flexibility
  https://github.com/dollabs/planviz/issues/24
- Added command line switch --strict to enforce schema checking
- Updated dependencies

### 0.8.7 - 2016-09-30

Changed
- Updated dependencies
- Repositioned label and sequence-label notations
- Activities are shown with :name (if that slot was set)
  else derived based on ui/construct-label
- Now support a more elaborate edge tool tip which includes
  plant, plantid, interface, argsmap, cost, reward, controllable

### 0.8.6 - 2016-06-03

Added
- Added --auto command line argument to start in /auto mode
- Now fully respect --port argument (and log to port specific log files)
- Updated dependencies
- Subscribe to RMQ auto-ack true
- Added --log-level command line argument
- Quiet boot output (if not running from jar)
- Removed text shadow (to improve legibility)
- Development debugging (connect to PLANVIZ server on port 8080
  if in development mode on port 3000)
- Implemented help menu
- Fixed link-arc algorithm to be closer to to the activity edge (so bounds don't overlap)
- Fixed /normal to clear null-activites after a run
- Fixed highlighted and hidden edges visible.
- The aggregated ("fake") edge is now styled to be a "rollup" of subgraph states
- Now /auto mode will display the corresponding plan (HTN for TPN, or vice versa)
  Care is taken to insure that both parts of a merged plan pair (TPN=HTN) are
  loaded such that highlighting works as expected.
- Verified that the CSS units are explicitly "px" for the Plans component.
- Exit gracefully if the desired web server port is unavailable.
- Used the new element number to improve selection (highlighting) performance
- The planviz launcher script now respects JVM_OPTS
- Added support for TPN args and argsmap

### 0.8.5 - 2016-06-03

Added
- Show new activity attribute :controllable in the right click "edge information" menu.
- Add delay-activty edge type (updated plan-schema to 0.2.5)
- Updated CLJS dependency
- File pathname improvements (cwd now forwarded in options)
- Tune link-arc factors to differentiate constraint arcs

### [0.8.4] - 2016-05-24

Added
- Right click actions
- Updated dependencies
- Updated right-click menu handling functions (and corresponding user actions)
- Added facility to load general (or plan specific) URL's to open (on right click)
- TPN Hide/Show implemented (including auto mode)
- HTN Focus/Blur implemented (including auto mode)
- Coalesced plan related meta data in app/plans (removed app/selection)
- CSS: Redesigned TPN choice/parallel nodes for "hidden" style in chrome

### [0.8.3] - 2016-05-16

Changed
- Show between labels on activities (in parens)
- Show sequence-labels on nodes and activities (if present)
- Fixed edge/plant-id to be edge/plantid
- ui.cljs now calculates label width using the same algorithm as tplan.cljs
- Added `/export` function to save a plan as an SVG file
- Divided CSS files into those required for PLANVIZ (planviz.css)
  and for exporting to SVG (tplan.css)
- Properly annoted uncontrolled choice nodes with sum of probabilities
- Show choice cost, reward, probability and guard as tooltips
- Change routing key for PLANVIZ to **pamela.viz**
- Updated dependencies
- TPLAN: Move state nodes with slack to the right during rebalancing
- Added support for new constraints: :cost<=-constraint, :reward>=-constraint
  * Adapted link-arc to distinguish bounds, cost and reward constraints
    between the same nodes
  * new CSS constraint styles (red for costs, green for rewards)

### [0.8.2] - 2016-04-27

Changed
* Significant performance improvements
  * Optimized component queries (to be minimal)
  * Specified precise, minimal queries where full denormalization was unnecessary
  * Decomposed long running functions into several asynchronous tasks
  * Various ClojureScript performance improvements
* Fixed plan loading to pre-load corresponding TPN (when available)
* Added a hide/show tooltips function
* Reorganized actions.cljs function order
* Updated dependencies
* Removed vestiges of non graphic rendering support

### [0.8.1] - 2016-04-12

Changed
* Initial publication on github

### 0.1.0

Added
*  Initial version

[0.8.1]: https://github.com/dollabs/planviz/compare/0.1.0...0.8.1
[0.8.2]: https://github.com/dollabs/planviz/compare/0.8.1...0.8.2
[0.8.3]: https://github.com/dollabs/planviz/compare/0.8.2...0.8.3
[0.8.4]: https://github.com/dollabs/planviz/compare/0.8.3...0.8.4
[0.8.8]: https://github.com/dollabs/planviz/compare/0.8.4...0.8.8
[0.8.9]: https://github.com/dollabs/planviz/compare/0.8.8...0.8.9
[0.9.0]: https://github.com/dollabs/planviz/compare/0.8.9...0.9.0
[Unreleased]: https://github.com/dollabs/planviz/compare/0.9.0...HEAD
