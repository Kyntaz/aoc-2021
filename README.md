# 🦉 Advent of Code 2021 in Prolog

Solutions to the 2021 Advent of Code (https://adventofcode.com/2021/) in SWI-Prolog.

## Running a specific day and part

There is a script to run a specific part of a day.
To run write the following command in the root of the project:

```
./run.ps1 <day> <part>
```

Where day and part are the day and the part you want to run.
So for the fourth day, second part the command would be:

```
./run.ps1 d4 p2
```

## Structure

The project has one folder, `./dX`, for each day.
Each folder contains one prolog file called `dX.pl` where X is the number of the day.
This file should define one predicate `pY` for each part Y of the day.
There is also an `input.txt` file which contains the input for the day.
This input is fed to the main prolog file through the `stdin`.


The folder `./common` contains prolog modules with generally useful predicates and can be included to make life easier.

## Requirements

* SWI-Prolog
* PowerShell *(Optional)*
