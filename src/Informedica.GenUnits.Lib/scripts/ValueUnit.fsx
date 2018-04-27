#load @"references.fsx"

open MathNet.Numerics
open Informedica.GenUnits.Lib

CombiUnit.Units.massMicroGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeMinute 2N)
|> CombiUnit.toString
|> CombiUnit.fromString


let v1 =
    ValueUnit.create 5N (CombiUnit.Units.massMicroGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeMinute 1N))

let v2 =
    ValueUnit.create 100N (CombiUnit.Units.massMilliGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeHour 1N))

v2 / v1

v2
|> ValueUnit.convertTo (CombiUnit.Units.massMicroGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeMinute 1N))
|> (fun vu -> vu / v1)

open ValueUnit.Units

let toString = ValueUnit.toLangString Unit.Units.English 3

let (>==>) vu f =
    vu 
    |> toString
    |> printfn "%s"
    f vu

let mg = massMilliGram 1N
let mcg = massMicroGram 1N
let ml = volumeMilliLiter 1N
let kg = weightKg 1N
let hr = timeHour 1N
let min = timeMinute 1N
let mcgkgmin = (CombiUnit.Units.massMicroGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeMinute 1N))

(200N |> mg)
>==> (fun vu -> vu / (5N |> ml)) // Concentration
>==> (fun vu -> vu * (2N |> ml)) // take 2 ml
>==> (fun vu -> vu / (50N |> ml)) // dissolve in 50 ml
>==> (fun vu -> vu * ((1N |> ml) / (1N |> hr))) // infusion speed
>==> (fun vu -> vu / (5N |> kg)) // for 5 kg patient
>==> ValueUnit.convertTo mcgkgmin // Convert to dose unit
|> toString


