# Change Log

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

### [Unreleased]

Added
- Added --auto command line argument to start in /auto mode
- Now fully respect --port argument (and log to port specific log files)

### [0.8.5] - 2016-06-03

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
[0.8.4]: https://github.com/dollabs/planviz/compare/0.8.4...0.8.5
[Unreleased]: https://github.com/dollabs/planviz/compare/0.8.5...HEAD
