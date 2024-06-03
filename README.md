# Decomment
<img src="https://img.shields.io/badge/FSharp-7.0-purple"> <img src="https://img.shields.io/badge/NUnit-3.13.3-yellow"> <img src="https://img.shields.io/badge/DotNet-7.0-green"> <img src="https://img.shields.io/badge/License-MIT-blue">



This CLI tool removes comments from C#, F#, C++, Java, JavaScript, OCaml, Haskell, 
and Python source code. 

## Demo
<img src="https://github.com/hirotk/Decomment/assets/6882458/5d5bfaf8-fa15-4a17-9101-14020cc6240c" alt="demo" style="max-width:100%">


## Usage
Remove all comments.
```
% Decomment Foo.cs
```

## Features
- Remove all comments except for documentation comments.
- Replace all comments with indented empty lines.


Remove all comments except for document comments.
```
% Decomment -d Foo.cs
```

Replace all comments with indented empty lines.
```
% Decomment -e Foo.cs
```

Show help.
```
 % Decomment -h
Usage: Decomment [-d] [-e] <path>
    -d : remove comments except for document comments
    -e : replace comments with indented empty lines 
    <path> : path to a source code file
            (*.cs, *.fs, *.java *.cpp, *js, *.ml, *.hs, *.py)"

```

