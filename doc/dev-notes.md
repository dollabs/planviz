#### Updating display labels
  Send a message in the following format to routing key `tpn.object.update`

```
{
  "act-9": {
    "uid": "act-9",
    "tpn-object-state": "normal",
    "display-name": "hello label update"
  },
  "network-id": "net-3"
}
```  

#### `WIP` Setup Ideaj with cursive for CLJS development (Summary)
 * `boot -d onetom/boot-lein-generate generate` to generate lein project.clj file from build.boot
 * In Idea, import new project from existing sources.
 * Create new run configuration using nrepl [See Details](https://github.com/tmarble/tenzing3)
 * `boot cljs-dev`
 * Connect to nrepl from Ideaj

[Complete exmaple](https://github.com/tmarble/tenzing3)