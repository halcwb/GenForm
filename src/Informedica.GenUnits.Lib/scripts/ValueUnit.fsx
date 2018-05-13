#load @"references.fsx"

open MathNet.Numerics

open Informedica.GenUnits.Lib
open ValueUnit

let toString = toString Units.English Units.Short

let (>==>) vu f =
    vu 
    |> toString
    |> printfn "%s"
    f vu

let x = create Units.Count.times
let mg = create Units.Mass.milliGram
let mcg = create Units.Mass.microGram
let ml = create Units.Volume.milliLiter
let kg = create Units.Weight.kiloGram
let hr = create Units.Time.hour
let day = create Units.Time.day
let min = create Units.Time.minute

let mcgkgmin = Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.minute
let mgkgday = Units.Mass.milliGram |> per Units.Weight.kiloGram |> per Units.Time.day

(200N |> mg)
>==> (fun vu -> vu / (5N |> ml)) // Concentration
>==> (fun vu -> vu * (2N |> ml)) // take 2 ml
>==> (fun vu -> vu / (50N |> ml)) // dissolve in 50 ml
>==> (fun vu -> vu * ((1N |> ml) / (1N |> hr))) // infusion speed
>==> (fun vu -> vu / (5N |> kg)) // for 5 kg patient
>==> convertTo mcgkgmin // Convert to dose unit
>==> toString

(8N / 25N) |> create (Units.Mass.milliGram |> per Units.Time.hour |> per Units.Weight.kiloGram)
|> convertTo mcgkgmin
|> convertTo (Units.Mass.microGram |> per Units.Weight.kiloGram |> per Units.Time.minute)


((400N |> mg) /  (20N |> ml)) * (((10N |> ml) + (10N |> ml)) / (1N |> hr))


createCombiUnit (Count(Times(2N)), OpPer, Time(Hour(1N)))
|> (fun u -> createCombiUnit ((Mass(MilliGram(1N)), OpTimes, u)))

((20N |> mg) * ((3N |> x) / (1N |> day))) / (8N |> kg)
==> mgkgday

Api.eval "100 mg[Mass] * 1 gram[Mass]"
