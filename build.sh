#!/bin/bash

sass --style=compressed scss/sizeup.scss sizeup.min.css
cp sizeup.min.css public/

elm make src/Main.elm --optimize --output=main.js
cp main.js public/
cp index.html public/
