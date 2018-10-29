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
        |> Dosage.toString true



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
    open Informedica.GenUtils.Lib.BCL

    module Dosage = DoseRule.Dosage
    
    module RF = Informedica.GenProduct.Lib.RuleFinder 
    module DR = Informedica.GenProduct.Lib.DoseRule
    module GPP = Informedica.GenProduct.Lib.GenPresProduct


    let createDoseRules = GStand.createDoseRules false None None None None


    let toStr = DoseRule.toString false


    let printDoseRules rs = 
        rs
        |> Seq.iter (fun dr -> 
            dr 
            |> toStr
            |> printfn "%s\n"
        )


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
        |> printDoseRules

        createDoseRules "paracetamol" "" ""
        |> printDoseRules
         
        createDoseRules "gentamicine" "" "intraveneus"
        |> printDoseRules

        createDoseRules "fentanyl" "" ""
        |> printDoseRules

        createDoseRules "dopamine" "" "intraveneus"
        |> printDoseRules

        createDoseRules "salbutamol" "" "intraveneus"
        |> printDoseRules


        RF.createFilter None None None None "paracetamol" "" ""
        |> RF.find true
        |> getSubstanceDoses
        |> Seq.iter (fun (inds, sd) -> 
            printfn "Indication %s" (inds |> String.concat ", ")
            printfn "%s" (sd |> Dosage.toString true)
        )
 

        RF.createFilter None None None None "gentamicine" "" ""
        |> RF.find true
        |> getPatients
        |> Seq.iter (fun (pat, sds, _) -> 
            printfn "%s" (pat |> Patient.toString)
            sds
            |> Seq.iter (fun (inds, sd) -> 
            printfn "Indication %s" (inds |> String.concat ", ")
            printfn "%s" (sd |> Dosage.toString true)
            )
        )


        GStand.createDoseRules true (Some 2.) (Some 4.) None None "paracetamol" "" "oraal"
        |> printDoseRules

        DR.get ()
        |> Seq.filter (fun dr -> 
            dr.Freq.Frequency = 1. && 
            dr.Freq.Time = "per uur" &&
            dr.Routes = [|"intraveneus"|]
        )
        |> Seq.collect (fun dr -> dr.GenericProduct |> Seq.map (fun gp -> gp.Name))
        |> Seq.distinct
        |> Seq.sort
        |> Seq.iter (printfn "%s")
//        |> Seq.length

        DR.get ()
        |> Seq.filter (fun dr ->
            dr.GenericProduct 
            |> Seq.map (fun gp -> gp.Name)
            |> Seq.exists (String.startsWithCapsInsens "salbutamol")
        )
        //|> Seq.collect (fun dr -> 
        //    dr.GenericProduct
        //    |> Seq.map (fun gp -> gp.Name, dr.Routes)
        //)
        |> Seq.map (DR.toString ",")
        |> Seq.distinct
        |> Seq.iter (printfn "%A")

        DR.get ()
        |> Seq.filter (fun dr ->
            dr.GenericProduct 
            |> Seq.map (fun gp -> gp.Name)
            |> Seq.exists (String.startsWithCapsInsens "fentanyl") &&
            dr.Freq.Time |> String.startsWithCapsInsens "eenmalig"
        )
        //|> Seq.collect (fun dr -> 
        //    dr.GenericProduct
        //    |> Seq.map (fun gp -> gp.Name, dr.Routes)
        //)
        |> Seq.map (DR.toString ",")
        |> Seq.distinct
        |> Seq.iter (printfn "%A")

        DR.get ()
        |> Seq.filter (fun dr ->
            dr.GenericProduct 
            |> Seq.map (fun gp -> gp.Name)
            |> Seq.exists (String.startsWithCapsInsens "fentanyl") 
        )
        //|> Seq.collect (fun dr -> 
        //    dr.GenericProduct
        //    |> Seq.map (fun gp -> gp.Name, dr.Routes)
        //)
        |> Seq.map (fun dr -> dr.Freq.Time)
        |> Seq.distinct
        |> Seq.iter (printfn "%A")


        GPP.get true
        |> Seq.filter (fun gpp -> gpp.Name |> String.equalsCapInsens "salbutamol")
        |> Seq.iter (fun gpp -> 
            gpp 
            |> Informedica.GenProduct.Lib.GenPresProduct.toString
            |> (printfn "%s")
        )

        DR.get ()
        |> Seq.collect (fun r -> r.Routes)
        |> Seq.distinct
        |> Seq.sort
        |> Seq.iter (printfn "%s")

        GPP.get true
        |> Seq.filter (fun gpp -> 
            gpp.Route |> Seq.exists (fun r -> r |> String.equalsCapInsens "parenteraal")
        )
        |> Seq.distinct
        |> Seq.sort
        |> Seq.iter (GPP.toString >> printfn "%s")

        printfn "DoseRule Routes"
        DR.routes ()
        |> Seq.filter (fun r ->
            
            GPP.getRoutes ()
            |> Seq.exists (fun r' -> r = r') 
            |> not
        )
        |> Seq.sort
        |> Seq.iter (printfn "|%s|")
        printfn ""        
        printfn "GenPresProduct Routes"
        GPP.getRoutes ()
        |> Seq.filter (fun r ->
            DR.routes ()
            |> Seq.exists (fun r' -> r = r')
            |> not
        )
        |> Seq.sort
        |> Seq.iter (printfn "|%s|")


    GStand.createDoseRules false (Some 1.1) (Some 5.) None None "gentamicine" "" "intraveneus"
    |> printDoseRules

    
    
    //open MathNet.Numerics

    //module VU = Informedica.GenUnits.Lib.ValueUnit

    //1N |> VU.create VU.Units.Time.month |> VU.convertTo VU.Units.Time.month   
    