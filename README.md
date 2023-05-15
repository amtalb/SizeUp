# SizeUp
<p align="center"><img src="sizeup.png" /></p>


SizeUp is an Elm app that takes one country (or territory, department, breakaway state, etc.) and finds the country that most closely matches its area. This isn't a terribly sophisticated or groundbreaking tool but it is a bit interesting and it goes to show how distorted typical 2-dimensional maps are (Greenland is about the size of Saudi Arabia, not Africa!). Additionally, Elm is a delightful language that makes web programming actually fun. Go FP!

Check out the live site at [https://amtalb.github.io/SizeUp](https://amtalb.github.io/SizeUp)

### Usage
To build the project run:
```
elm make src/Main.elm --optimize --output=main.js
```

Or run the handy-dandy `build.sh` script to let it do all the work for you.

### Data
Country data is pulled in JSON form from mledoze's GitHub repo located [here](https://github.com/mledoze/countries).