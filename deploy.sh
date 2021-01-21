#!/bin/bash

sass --style=compressed scss/sizeup.scss sizeup.min.css
cp sizeup.min.css docs/

elm make src/Main.elm --optimize --output=main.js
cp main.js docs/
cp index.html docs/
