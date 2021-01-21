# SizeUp
<p align="center"><img src="sizeup.png" /></p>


SizeUp is an Elm app that takes one country (or territory, department, breakaway state, etc.) and finds the country that most closely matches its area. This isn't a terribly sophisticated or groundbreaking tool but it is a bit interesting and it goes to show how distorted typical 2-dimensional maps are (Greenland is about the size of Saudi Arabia, not Africa!).

Check out the live site at [https://amtalb.github.io/SizeUp](https://amtalb.github.io/SizeUp)

## Usage
To build the project run:
```
elm make src/Main.elm --optimize --output=main.js
```

This project uses Firebase as a backend. The data file is stored in a Firebase Realtime Database. If you want to get your own version of this project running, you can use my database or build your own with the `data/data.json` file.
