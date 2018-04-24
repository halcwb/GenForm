#load "genunits-files.fsx"

#time 

open MathNet.Numerics
open Informedica.GenUnits.Lib


Unit.Units.MilliGram 
|> Unit.Units.toMass
|> CombiUnit.create 20N
|> CombiUnit.per 1N (Unit.Units.MilliLiter |> Unit.Units.toVolume)


Unit.Units.Times 
|> Unit.Units.toCount
|> CombiUnit.create 1N
|> CombiUnit.per 3N (Unit.Units.Day |> Unit.Units.toTime)
|> ValueUnit.create 2N
|> ValueUnit.convertTo (
    Unit.Units.Times 
    |> Unit.Units.toCount
    |> CombiUnit.create 1N
    |> CombiUnit.per 1N (Unit.Units.Week |> Unit.Units.toTime)
)
|> ValueUnit.toLangString Unit.Units.DutchLong 2
