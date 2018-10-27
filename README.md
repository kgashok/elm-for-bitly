Simple Elm Sample App
=========================

The Counter example from the [Elm documentation](https://guide.elm-lang.org/) in glitch just to see if it would work.

## Steps to Get Here

* add elm (v0.18.0) to the package.json
* open the console and check to make sure elm-repl works
* ran a `elm-make` manually to trigger elm dependency check
* added `source/main.elm` to project
* updated `package.json` to run `npm prestart` on start and modified `prestart` to run `elm-make source/main.elm --output=public/elm.js`
* modified `index.html` to reference `elm.js` and start the Main module in it with `var main = Elm.Main.fullscreen();`
* added watch.json to restart site when any `.eml` files are modified
* added `elm-stuff` folder to `.gitignore` so it doesn't pollute the file list (left `exact-dependencies.json` visible)

## Updated to elm v0.19.0

* updated the elm npm package to v0.19.0-bugfix2
* add elm-upgrade and elm-format
* run elm-upgrade

## Command line changes from elm v0.18.0 to v0.19.0 

taken from the [release notes](https://github.com/elm/compiler/blob/master/upgrade-docs/0.19.md)

```
# 0.19         # 0.18
elm make       # elm-make
elm repl       # elm-repl
elm reactor    # elm-reactor
elm install    # elm-package install
elm publish    # elm-package publish
elm bump       # elm-package bump
elm diff       # elm-package diff
```

\ ゜o゜)ノ
