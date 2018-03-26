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
// = 
>>! (1N.toCU Mass_MicroGram) / (1N.toCU Weight_KiloGram) / (1N.toCU Time_Minute)
|> toString
>=> ignore
 



// calculate morfine infusion for 10 kg weight
// take 1 ml of morfine 10 mg/1 ml ampul
// (1 ml * 10 mg / 1 ml) = 10 mg
(1N.toVU Volume_MilliLiter) * ((10N.toVU Mass_MilliGram) / (1N.toVU Volume_MilliLiter))
|> toString
>=> fromString
// add this to 49 ml of saline
// 10 mg / (1 ml + 49 ml) = 0.2 mg / ml
|> div ((1N.toVU Volume_MilliLiter) + (49N.toVU Volume_MilliLiter))
|> toString
>=> fromString
// infuse with rate 1 ml/hour
// 0.2 mg/ml * 1 ml/hour = 0.2 mg/hour
|> times ((1N.toVU Volume_MilliLiter) / (1N.toVU Time_Hour))
|> toString
>=> fromString
// calculate dose for 10 kg in mcg/kg/min
// 0.2 mg/hour / 10 kg = 0.02 mg/kg/hour
|> div (10N.toVU Weight_KiloGram)
// convert to mcg/kg/hour
// = 20 mcg/kg/hour
>>! (1N.toCU Mass_MicroGram) / (1N.toCU Weight_KiloGram) / (1N.toCU Time_Hour)
|> toString
>=> fromString
// or convert to mg/kg/dag
// = 0.48 mg/kg/day
>>! (1N.toCU Mass_MilliGram) / ((1N.toCU Weight_KiloGram) / 1N.toCU Time_Day)
|> toString
>=> ignore

// prints out
// "10 mg[Mass]"
// "1/5 mg[Mass]/ml[Volume]"
// "1/5 mg[Mass]/hr[Time]"
// "20 mcg[Mass]/kg[Weight]/hr[Time]"
// "12/25 mg[Mass]/kg[Weight]/day[Time]" 
// = 0.48 mg/kg/day or 10 mcg/kg/hour



// calculate gentamicin infusion 2 X per 3 day for 1.6 kg weight
// take 1 ml of gentamicin 10 mg/1 ml ampul
// (0.8 ml * 10 mg / 1 ml) = 8 mg
((8N / 10N).toVU Volume_MilliLiter) * ((10N.toVU Mass_MilliGram) / (1N.toVU Volume_MilliLiter))
|> toString
>=> fromString
// add this to 4.2 ml of saline
// 8 mg / (0.8 ml + 4.2 ml) = 1.6 mg / ml
|> div (((8N / 10N).toVU Volume_MilliLiter) + ((42N / 10N).toVU Volume_MilliLiter))
|> toString
>=> fromString
// administer 2 x  5 ml per 3 day
|> times ((2N.toVU Count_Times) * (5N.toVU Volume_MilliLiter) / (3N.toVU Time_Day))
|> toString
>=> fromString
// calculate dose for 1.6 kg in mg/kg/day
// 0.2 mg/hour / 10 kg = 0.02 mg/kg/hour
|> div ((16N / 10N).toVU Weight_KiloGram)
// convert to mg/kg/36 hour
// = 5 mg/kg/36 hour
>>! (1N.toCU Mass_MilliGram) / (1N.toCU Weight_KiloGram) / (36N.toCU Time_Hour)
|> toString
>=> ignore

// prints out
// "8 mg[Mass]"
// "8/5 mg[Mass]/ml[Volume]"
// "16/3 mg[Mass]/day[Time]"
// "5 mg[Mass]/kg[Weight]/36 hr[Time]"
// = 5 mg/kg/36 hour