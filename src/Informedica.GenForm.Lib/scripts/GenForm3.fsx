#I __SOURCE_DIRECTORY__

#load "./../../../.paket/load/netstandard2.0/main.group.fsx"

#r "./../../Informedica.GenUtils.Lib/bin/Debug/netstandard2.0/Informedica.GenUtils.Lib.dll"
#r "./../../Informedica.GenUnits.Lib/bin/Debug/netstandard2.0/Informedica.GenUnits.Lib.dll"
#r "./../../Informedica.GenProduct.Lib/bin/Debug/netstandard2.0/Informedica.GenProduct.Lib.dll"

#r "netstandard"


#time


open System

let pwd = Environment.GetEnvironmentVariable("HOME")
Environment.CurrentDirectory <- 
    if pwd |> String.IsNullOrWhiteSpace then 
        __SOURCE_DIRECTORY__ + "/../../../"

    else 
        pwd + "/Development/GenForm/" //__SOURCE_DIRECTORY__ + "/../../../"


#load "./../Utils/String.fs" 
#load "./../Utils/List.fs" 
#load "./../Mapping.fs" 
#load "./../ValueUnit.fs" 
#load "./../MinMax.fs" 
#load "./../Route.fs" 
#load "./../Product.fs" 
#load "./../Patient.fs" 
#load "./../DoseRule.fs" 
#load "./../GStand.fs" 
#load "./../Dto.fs" 


open Aether  
open Aether.Optics

open Informedica.GenForm.Lib


module MinMaxTests =

    module MinMax = MinMax.Optics

    let v1, v2 = 
        ValueUnit.substanceInGStandUnit 10. "milligram" |> Option.get ,
        ValueUnit.substanceInGStandUnit 20. "milligram" |> Option.get
        
    let incl1, incl2 =
        v1 |> MinMax.inclusive, 
        v2 |> MinMax.inclusive

    let v3, v4 = 
        ValueUnit.substanceInGStandUnit 30. "milligram" |> Option.get ,
        ValueUnit.substanceInGStandUnit 40. "milligram" |> Option.get
        
    let incl3, incl4 =
        v3 |> MinMax.inclusive, 
        v4 |> MinMax.inclusive


    let toString () =
        MinMax.empty
        |> MinMax.setMin (ValueUnit.createFromGStand 1. "milligram" |> Option.get |> MinMax.Inclusive)
        |> MinMax.setMax (ValueUnit.createFromGStand 10. "milligram" |> Option.get |> MinMax.Inclusive)
        |> MinMax.toString
        
        
    let a1, a2 =
        0.1 |> ValueUnit.ageInMo |> Option.bind (MinMax.Inclusive >> Some) |> Option.get ,
        0.1 |> ValueUnit.ageInYr |> Option.bind (MinMax.Exclusive >> Some) |> Option.get
        
        
    let ageToString () =
        MinMax.empty
        |> MinMax.setMin a1
        |> MinMax.setMax a2
        |> MinMax.ageToString


    let valueComp () =

        printfn "%A < %A = %A" incl1 incl2 (MinMax.valueST incl1 incl2)
        printfn "%A < %A = %A" incl1 incl1 (MinMax.valueST incl1 incl1)
        printfn "%A <= %A = %A" incl1 incl2 (MinMax.valueSTE incl1 incl2)
        printfn "%A <= %A = %A" incl1 incl1 (MinMax.valueSTE incl1 incl1)
        printfn ""
        printfn "%A > %A = %A" incl1 incl2 (MinMax.valueLT incl1 incl2)
        printfn "%A > %A = %A" incl1 incl1 (MinMax.valueLT incl1 incl1)
        printfn "%A >= %A = %A" incl1 incl2 (MinMax.valueLTE incl1 incl2)
        printfn "%A >= %A = %A" incl1 incl1 (MinMax.valueLTE incl1 incl1)


    // ToDo handle None cases correctly?
    let testFold () =
        let mms = 
            [
                MinMax.empty
                MinMax.empty |> MinMax.setMin incl1
                MinMax.empty |> MinMax.setMin incl2
                MinMax.empty |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMax incl4
                MinMax.empty |> MinMax.setMin incl1 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl2 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl3 |> MinMax.setMax incl3
                MinMax.empty |> MinMax.setMin incl4 |> MinMax.setMax incl4
            ]

        mms
        |> MinMax.foldMaximize
        |> MinMax.toString
        |> printfn "Maximized:\n%s"


        mms
        |> MinMax.foldMinimize
        |> MinMax.toString
        |> printfn "Minimized:\n%s"


    let inRange () =
        let mm1 = MinMax.empty
        let mm2 = 
            MinMax.empty
            |> MinMax.setMin incl1
        let mm3 = 
            MinMax.empty
            |> MinMax.setMax incl4
        let mm4 =
            MinMax.empty
            |> MinMax.setMin incl2
            |> MinMax.setMax incl3

        let test v mm =
            printfn "%A in range: %A = %A" (v |> MinMax.valueToString) (mm |> MinMax.toString) (MinMax.inRange v mm)


        [
            (incl1, mm1)
            (incl2, mm1)
            (incl3, mm1)
            (incl4, mm1)
            (incl1, mm2)
            (incl2, mm2)
            (incl3, mm2)
            (incl4, mm2)
            (incl1, mm3)
            (incl2, mm3)
            (incl3, mm3)
            (incl4, mm3)
            (incl1, mm4)
            (incl2, mm4)
            (incl3, mm4)
            (incl4, mm4)
        ]
        |> List.iter (fun (v, mm) -> test v mm)

        


module DoseRangeTests =

    module DoseRange = DoseRule.DoseRange

    let setMaxNormDose = Optic.set DoseRange.Optics.inclMaxNormLens
    let setMaxAbsDose = Optic.set DoseRange.Optics.inclMaxAbsLens

    let toString () =
        DoseRange.empty
        |> setMaxNormDose (ValueUnit.createFromGStand 10. "milligram")
        |> setMaxAbsDose (ValueUnit.createFromGStand 100. "milligram")
        |> DoseRange.toString



module DosageTests =
    
    module Dosage = DoseRule.Dosage

    let setNormMinStartDose = Optic.set Dosage.Optics.inclMinNormStartDosagePrism
    let setAbsMaxStartDose = Optic.set Dosage.Optics.inclMaxAbsStartDosagePrism

    let setNormMinSingleDose = Optic.set Dosage.Optics.inclMinNormSingleDosagePrism
    let setAbsMaxSingleDose = Optic.set Dosage.Optics.inclMaxAbsSingleDosagePrism

    let toString () =
        Dosage.empty
        |> setNormMinStartDose (ValueUnit.createFromGStand 10. "milligram")
        |> setAbsMaxStartDose (ValueUnit.createFromGStand 1. "gram")
        |> setNormMinSingleDose (ValueUnit.createFromGStand 10. "milligram")
        |> setAbsMaxSingleDose (ValueUnit.createFromGStand 1. "gram")
        |> Dosage.toString 



module PatientTests =

    module Patient = Patient.Optics

    type Patient = Patient.Patient
    
    let toString () =
        Patient.empty
        |> Patient.setInclMinGestAge (28.  |> ValueUnit.ageInWk)
        |> Patient.setExclMaxGestAge (33.  |> ValueUnit.ageInWk)
        |> Patient.setExclMinAge (1. |> ValueUnit.ageInMo)
        |> Patient.setInclMaxAge (120. |> ValueUnit.ageInWk)
        |> Patient.setInclMinWeight (0.15  |> ValueUnit.weightInKg)
        |> Patient.setInclMaxWeight (4.00  |> ValueUnit.weightInKg)
        |> Patient.setInclMinBSA (0.15  |> ValueUnit.bsaInM2)
        |> Patient.setInclMaxBSA (1.00  |> ValueUnit.bsaInM2)
        |> (fun p -> p |> (Optic.set Patient.Gender_) Patient.Male)
        |> Patient.toString




module GStandTests =

    open GStand

    module Dosage = DoseRule.Dosage
    
    module RF = Informedica.GenProduct.Lib.RuleFinder 
    module DR = Informedica.GenProduct.Lib.DoseRule

    let createDoseRules = GStand.createDoseRules None None None None


    let mapFrequency () =
        DR.get ()
        |> Seq.map (fun dr -> dr.Freq)
        |> Seq.distinct
        |> Seq.sortBy (fun fr -> fr.Time, fr.Frequency)
        |> Seq.map (fun fr -> fr, fr |> GStand.mapFreq)
        |> Seq.iter (fun (fr, vu) ->
            printfn "%A %s = %s" fr.Frequency fr.Time (vu |> ValueUnit.toStringPrec 0)
        )

    let tests () =
        createDoseRules "trimethoprim/sulfamethoxazol" "" ""
        |> Seq.iter (fun dr -> 
            dr 
            |> DoseRule.toString
            |> printfn "%s\n"
        )

        createDoseRules "paracetamol" "" ""
        |> Seq.iter (fun dr ->
            dr 
            |> DoseRule.toString
            |> printfn "%s\n"
        )
         
        createDoseRules "gentamicine" "" "iv"
        |> Seq.iter (fun dr ->
            dr 
            |> DoseRule.toString
            |> printfn "%s\n"
        )

        createDoseRules "fentanyl" "" "iv"
        |> Seq.iter (fun dr ->
            dr 
            |> DoseRule.toString
            |> printfn "%s\n"
        )

        createDoseRules "dopamine" "" "iv"
        |> Seq.iter (fun dr ->
            dr 
            |> DoseRule.toString
            |> printfn "%s\n"
        )



        RF.createFilter None None None None "paracetamol" "" ""
        |> RF.find
        |> getSubstanceDoses
        |> Seq.iter (fun (inds, sd) -> 
            printfn "Indication %s" (inds |> String.concat ", ")
            printfn "%s" (sd |> Dosage.toString)
        )
 

        RF.createFilter None None None None "gentamicine" "" ""
        |> RF.find
        |> getPatients
        |> Seq.iter (fun (pat, sds, _) -> 
            printfn "%s" (pat |> Patient.toString)
            sds
            |> Seq.iter (fun (inds, sd) -> 
            printfn "Indication %s" (inds |> String.concat ", ")
            printfn "%s" (sd |> Dosage.toString)
            )
        )


