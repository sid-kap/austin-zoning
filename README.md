# Austin Zoning Map

*Purpose*: I wanted to better understand the zoning rules in Austin. The Austin city website does have an [interactive zoning map](http://www.austintexas.gov/GIS/ZoningProfile/), but it only allows you to look at the zoning details of one building at a time. (You have to click on a place on the map to see its zoning details.) I wanted to be able to see the zoning codes for all locations at once, so I built this tool using the same API as the Austin website.

# Installation
This project is written in Haskell and Typescript.

To build, you must have `stack`, `webpack`, and `yarn` on your computer.

Anyway, assuming you have the dependencies, you must do
```
stack build
pushd typescript-austin-zoning
yarn
webpack
popd
stack exec austin-zoning
```

The last command above starts the Haskell server on `localhost:3000` that serves both the static content (`/index.html` and `/bundle.js`) and the dynamic content (at `/area`).
