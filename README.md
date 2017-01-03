# Austin Zoning Map

*Purpose*: I wanted to better understand the zoning rules in Austin. The Austin city website does have an [interactive zoning map](http://www.austintexas.gov/GIS/ZoningProfile/), but it only allows you to look at the zoning details of one building at a time. (You have to click on a place on the map to see its zoning details.) I wanted to be able to see the zoning codes for all locations at once, so I built this tool using the same API as the Austin website.

# Installation
This project is written in Haskell and Typescript.

To run, you must have `stack` and `webpack` on your computer. You also should have all the required JavaScript dependencies stored in `typescript-austin-zoning/node_modules` (by going into `typescript-austin-zoning` and running `npm install` for each of the JavaScript dependencies). Currently the JS dependencies are not documented anywhere in this repo, so this step isn't possible for you. I intend to add them to the repo at some time (using a Yarn lockfile, maybe?) so that other people can build the project.

Anyway, assuming you have the dependencies, you must do
```
stack build
pushd typescript-austin-zoning
webpack
popd
stack exec austin-zoning
```

The last command above starts the Haskell server on `localhost:3000` that serves both the static content (`/index.html` and `/bundle.js`) and the dynamic content (at `/area`).
