#load "references.fsx"

open Informedica.GenUnits.Lib.Api
open MathNet.Numerics

let (>=>) x f = printfn "%A" x; f x
let inline div x1 x2 = x2 / x1
let inline times x1 x2 = x2 * x1

// calculates prescription
// dopamine 200 mg / 50 mL, 1 ml / hour for weight = 5 kg = 13.3 microg/kg/min
(200N.toVU Mass_MilliGram / 50N.toVU Volume_MilliLiter) * (1N.toVU Volume_MilliLiter / 1N.toVU Time_Hour) / (5N.toVU Weight_KiloGram)
>>! (1N.toCU Mass_MicroGram) / (1N.toCU Weight_KiloGram) / (1N.toCU Time_Minute)
|> toString

// calculates
// prescription gentamicin 5 mg/kg/2 days for weight = 2 kg
(10N.toVU Mass_MilliGram) / (2N.toVU Weight_KiloGram) / (2N.toVU Time_Day)
>>! (1N.toCU Mass_MilliGram) / (1N.toCU Weight_KiloGram) / (2N.toCU Time_Day)
|> toString


// 5 mg/kg/2 day
((5N.toVU Mass_MilliGram) / (1N.toVU Weight_KiloGram) / (2N.toVU Time_Day))
|> toString
>=> fromString
// divide by 1x/day
|> div ((1N.toVU Count_Times) / (1N.toVU Time_Day))
|> toString
>=> fromString
// times 1200 gram
|> times (1200N.toVU Weight_Gram)
|> toString
// print result
>=> ignore

// 72 mg/kg/day
((72N.toVU Mass_MilliGram) / (1N.toVU Weight_KiloGram) / (1N.toVU Time_Day))
|> toString
>=> fromString
// times 10 kg weight
|> times (10N.toVU Weight_KiloGram)
|> toString
>=> fromString
// divide by 3x/day
|> div ((3N.toVU Count_Times) / (1N.toVU Time_Day))
|> toString
>=> ignore

// 2 ml + 48 ml = 50 ml
(2N.toVU Volume_MilliLiter) + (48N.toVU Volume_MilliLiter)
|> toString

// 2000 ml + 48 l = 50 l
(2000N.toVU Volume_MilliLiter) + (48N.toVU Volume_Liter)
|> toString


// calculate dopamine infusion for 10 kg weight
// take 5 ml of dopamine 200 mg/5 ml ampul
// (5 ml * 200 mg / 5 ml) = 200 mg
(5N.toVU Volume_MilliLiter) * ((200N.toVU Mass_MilliGram) / (5N.toVU Volume_MilliLiter))
|> toString
>=> fromString
// add this to 45 ml of saline
// 200 mg / (5 ml + 45 ml) = 4 mg / ml
|> div ((5N.toVU Volume_MilliLiter) + (45N.toVU Volume_MilliLiter))
|> toString
>=> fromString
// infuse with rate 1 ml/hour
// 4 mg/ml * 1 ml/hour = 4 mg/hour
|> times ((1N.toVU Volume_MilliLiter) / (1N.toVU Time_Hour))
|> toString
>=> fromString
// calculate dose for 10 kg in mcg/kg/min
// 4 mg/hour / 10 kg = 20/3 mcg/kg/min ~ 6.7 mcg/kg/min
|> div (10N.toVU Weight_KiloGram)
// convert to mcg/kg/min
>>! (1N.toCU Mass_MicroGram) / (1N.toCU Weight_KiloGram) / (1N.toCU Time_Minute)
|> toString
>=> ignore
 