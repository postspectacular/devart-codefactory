# DevArt Co(de)Factory

![CodeFactory @ Barbican](assets/codefactory.jpg)

To view the live project and create your own piece, visit: http://devartcodefactory.com/

## Building this project

### Module overview

The project uses multiple source folders to group its various modules:

#### src-cljs

This folder contains the entire frontend source code, written in ClojureScript. It requires no 3rd party libraries apart from those listed (and automatically downloaded by Leiningen) in `project.clj`. The main application and entry point is in the `codefactory.app` namespace.

#### src-fabricate

This folder contains a complete Leiningen project and the source code to generate 3d models & render scenes (for [LuxRender](http://luxrender.net)) of the physical exhibit plan, as well as the generator for the 446 3D printed tiles used to create the 2.4 x 3.0 meter large flute structure and the cladding for the plinths holding the Nexus 10 tablets in the gallery. The 3D STL files and technical drawings for these structures are located in the `/assets` folder.

#### src-gae

Contains the complete Clojure backend and wrapper for the Google AppEngine API & core services. The main app itself is written as a standard [Ring](https://github.com/ring-clojure/ring) handler using [Compojure](https://github.com/weavejester/compojure). The AppEngine wrapper started out using some ideas & snippets from the outdated [appengine-magic](https://github.com/gcv/appengine-magic) project, but ended up differing quite substantially and might be developed further...

#### src-html

The source versions of the various HTML files used for the online version, Barbican gallery kiosks & workshops run. There're only minor differences between each (mainly config settings). All development is supposed to only happen with the `staging.html` file.

#### src-less

All stylesheets used by the webapp, defined using LESS. The file `minxins.less` contains various reusable snippets.

#### src-rpi

Python source code for the Raspberry PI to control the DMX gallery lighting & LCD panels displaying object credits.

### Requirements

* Google AppEngine 1.9.8
* Java 1.7
* Leiningen 2.4.0
* Clojure 1.6.0
* ClojureScript 0.0-2280
* lein-cljsbuild 1.0.3
* Grunt 0.4.5

### Running locally

```bash
# download & install AppEngine dependencies into local /lib dir
./install-dependencies.sh

# build & compress HTML & CSS
grunt less:prod htmlmin:prod replace

# compile ClojureScript/JS app
lein do cljsbuild clean, cljsbuild once prod

# launch AppEngine dev server
./launch.sh
```

Then open your browser and navigate to http://localhost:8080/staging/

If you end up hacking the ClojureScript source (in `/src-cljs`) and want (almost) instant automatic recompilation, open a separate terminal and use this invocation of `cljsbuild` instead:

```bash
lein do cljsbuild clean, cljsbuild auto dev
```

#### Google API key

For each submitted piece an unique URL is created, which due to the nature of UUIDs is not human readable. Therefore the API handler responsible makes use of Google's `goo.gl` URL shortener to create shorter versions, but requires a valid Google API key to do so. You can run the app without such a key, though in this case your objects can only be accessed via their original long URL.

## License

This project is licensed under the [Apache Software License 2.0](http://www.apache.org/licenses/LICENSE-2.0).

(c) 2014 Karsten Schmidt
