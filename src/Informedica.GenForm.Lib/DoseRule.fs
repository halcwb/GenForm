namespace Informedica.GenForm.Lib


module DoseRule =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib


    type Name = string


    type Generic = string


    type ATC = string


    type TherapyGroup = string


    type TherapySubGroup = string


    type Indication = string


    type Id = int


    module MinMax =

        open Aether

        type MinMax = 
            {
                Min : ValueUnit.ValueUnit Option 
                Max : ValueUnit.ValueUnit Option
            }

        let empty = { Min = None; Max = None }


        let toString { Min = min; Max = max } = 
            let toStr = ValueUnit.toLangString Unit.Units.DutchShort 2
            let s =
                match min, max with
                | None, None -> ""
                | Some min, None -> "vanaf " + (min |> toStr)
                | None, Some max ->
                    "tot " + (max |> toStr)
                | Some min, Some max -> (min |> toStr) + " - " + (max |> toStr)
            if s = "" then "" else s + " "


        let ageMin min minmax =
            { minmax with Min = ValueUnit.ageInMo min } 


        let ageMax max minmax =
            { minmax with Max = ValueUnit.ageInMo max } 


        let ageMinMax min max =
            empty
            |> ageMin min
            |> ageMax max


        let substanceMin min u minmax =
            { minmax with Min = ValueUnit.substanceInGStandUnit min u }


        let substanceMax max u minmax =
            { minmax with Max = ValueUnit.substanceInGStandUnit max u }


        let substanceMinMax min max u =
            empty
            |> substanceMin min u
            |> substanceMax max u


        let fold mms =
            mms |> List.fold (fun mm acc ->
                {  mm with
                    Min = Option.min acc.Min mm.Min
                    Max = Option.max acc.Max mm.Max
                }
            ) empty


        let inRange n { Min = min; Max = max } =
            if n |> Option.isNone then true
            else
                let n = n |> Option.get
                match min, max with
                | None, None -> true
                | Some min, None -> n >= min
                | None, Some max -> n <= max
                | Some min, Some max -> n >= min && n <= max


        type MinMax with

            static member Min_ =
                (fun mm -> mm.Min), (fun vu mm -> { mm with Min = vu })

            static member Max_ =
                (fun mm -> mm.Max), (fun vu mm -> { mm with Max = vu })


        let getMin = Optic.get MinMax.Min_


        let setMin vu = Optic.set MinMax.Min_ vu


        let getMax = Optic.get MinMax.Max_


        let setMax vu = Optic.set MinMax.Max_ vu



    module Patient = 

        open Aether
        open Aether.Operators

        type Patient = 
            {
                Age :     MinMax.MinMax
                Weight :  MinMax.MinMax
                BSA :     MinMax.MinMax
                GestAge : MinMax.MinMax
                Gender :  Patient.Gender Option
            }


        let empty = 
            {
                Age     = MinMax.empty
                Weight  = MinMax.empty
                BSA     = MinMax.empty
                GestAge = MinMax.empty
                Gender  = None
            }


        let toString p =
            let print label (minmax : MinMax.MinMax) s =
                 match minmax.Min, minmax.Max with
                 | None, None -> s
                 | _ -> s + label + ": " + (minmax |> MinMax.toString)
            if p = empty then "alle patienten"
            else 
                ""
                |> print "Leeftijd" p.Age
                |> print "Gewicht" p.Weight

        type Patient with

            static member Age_ =
                (fun pat -> pat.Age), (fun age pat -> { pat with Age = age } )

            static member Weight_ =
                (fun pat -> pat.Weight), (fun wgt pat -> { pat with Weight = wgt } )

            static member BSA_ =
                (fun pat -> pat.BSA), (fun bsa pat -> { pat with BSA = bsa } )

            static member GestAge_ =
                (fun pat -> pat.GestAge), (fun gst pat -> { pat with GestAge = gst } )

            static member Gender_ =
                (fun pat -> pat.Gender), (fun gnd pat -> { pat with Gender = gnd } )


        // === Age SETTERS GETTERS ==

        let getAge = Optic.get Patient.Age_


        let setAge = Optic.set Patient.Age_


        let minAgeLens = Patient.Age_ >-> MinMax.MinMax.Min_


        let getMinAge = Optic.get minAgeLens
 

        let setMinAge = Optic.set minAgeLens


        let setMinAgeMonths min pat = Optic.set minAgeLens (min |> ValueUnit.ageInMo) pat


        let maxAgeLens = Patient.Age_ >-> MinMax.MinMax.Max_


        let getMaxAge = Optic.get maxAgeLens
 

        let setMaxAge = Optic.set maxAgeLens


        let setMaxAgeMonths max pat = Optic.set maxAgeLens (max |> ValueUnit.ageInMo) pat


        // === Weight SETTERS GETTERS ==

        let getWeight = Optic.get Patient.Weight_


        let setWeight = Optic.set Patient.Weight_


        let minWeightLens = Patient.Weight_ >-> MinMax.MinMax.Min_


        let getMinWeight = Optic.get minWeightLens


        let setMinWeightKg min pat = Optic.set minWeightLens (min |> ValueUnit.weightInKg) pat
 

        let setWeightAge = Optic.set minWeightLens


        let maxWeightLens = Patient.Weight_ >-> MinMax.MinMax.Max_


        let getMaxWeight = Optic.get maxWeightLens
 

        let setMaxWeight = Optic.set maxWeightLens


        let setMaxWeightKg max pat = Optic.set maxWeightLens (max |> ValueUnit.weightInKg) pat



    module Dose =

        open Aether
        open Aether.Operators


        type Dose = 
            {
                /// The dose that can be given 
                /// each time
                PerTime : DoseValue Option
                /// For each frequency time period
                /// there is a specific dose
                PerPeriod : (FreqPeriod * DoseValue) list
            }
        and FreqPeriod = BigRational list * TimePeriod
        and DoseValue = 
            {
                /// The optional min/max total dose limits
                NormDose :      MinMax.MinMax
                /// The optional min/max absolute total dose limits
                AbsDose :       MinMax.MinMax
                /// The optional min/max total dose limits adjusted for body weight 
                NormDosePerKg : MinMax.MinMax
                /// The optional min/max absolute total dose limits adjusted for body weight 
                AbsDosePerKg :  MinMax.MinMax
                /// The optional min/max total dose limits adjusted for body surface area 
                NormDosePerM2 : MinMax.MinMax
                /// The optional min/max absolute total dose limits adjusted for body surface area 
                AbsDosePerM2 :  MinMax.MinMax
            }
        and TimePeriod = CombiUnit.CombiUnit


        let empty = 
            {
                PerTime = None
                PerPeriod = []
            }


        let emptyDoseValue = 
            {
                NormDose      = MinMax.empty
                AbsDose       = MinMax.empty
                NormDosePerKg = MinMax.empty
                AbsDosePerKg  = MinMax.empty
                NormDosePerM2 = MinMax.empty
                AbsDosePerM2  = MinMax.empty
            }


        let createFreqs sl =
            sl
            |> List.map (Mapping.mapFreq Mapping.GStandMap Mapping.GenFormMap)
            |> List.filter ((<>) "")
            |> List.map (fun s ->
                match s |> String.split "/" with
                | [t;u] ->
                    (t |> ValueUnit.fromString ,
                     u |> CombiUnit.fromString)
                    |> Some
                | _ -> None
            ) 
            |> List.filter Option.isSome
            |> List.map Option.get
            |> List.groupBy (fun (n, u) ->
                u
            )
            |> List.map (fun (u, ns) ->
                (ns 
                |> List.map (fst >> ValueUnit.get >> fst) ), u
            )


        type DoseValue with
            
            static member NormDose_ =
                (fun dv -> dv.NormDose), (fun nd dv -> { dv with NormDose = nd })
            
            static member AbsDose_ =
                (fun dv -> dv.AbsDose), (fun ad dv -> { dv with AbsDose = ad })
            
            static member NormDosePerKg_ =
                (fun dv -> dv.NormDosePerKg), (fun nd dv -> { dv with NormDosePerKg = nd })
            
            static member AbsDosePerKg_ =
                (fun dv -> dv.AbsDosePerKg), (fun ad dv -> { dv with AbsDosePerKg = ad })
            
            static member NormDosePerM2_ =
                (fun dv -> dv.NormDosePerM2), (fun nd dv -> { dv with NormDosePerM2 = nd })
            
            static member AbsDosePerM2_ =
                (fun dv -> dv.AbsDosePerM2), (fun ad dv -> { dv with AbsDosePerM2 = ad })

        
        let getNormDose = Optic.get DoseValue.NormDose_


        let setNormDose = Optic.set DoseValue.NormDose_


        let normDoseMinLens = DoseValue.NormDose_ >-> MinMax.MinMax.Min_

        
        let getNormDoseMin = Optic.get normDoseMinLens


        let setNormDoseMin = Optic.set normDoseMinLens


        let setNormDoseMinGStand v u dv = 
            dv 
            |>  setNormDoseMin (ValueUnit.substanceInGStandUnit v u)


        let normDoseMaxLens = DoseValue.NormDose_ >-> MinMax.MinMax.Max_

        
        let getNormDoseMax = Optic.get normDoseMaxLens


        let setNormDoseMax = Optic.set normDoseMaxLens


        let setNormDoseMaxGStand v u dv = 
            dv 
            |>  setNormDoseMax (ValueUnit.substanceInGStandUnit v u)

        
        let getAbsDose = Optic.get DoseValue.AbsDose_


        let setAbsDose = Optic.set DoseValue.AbsDose_


        let absDoseMinLens = DoseValue.AbsDose_ >-> MinMax.MinMax.Min_

        
        let getAbsDoseMin = Optic.get absDoseMinLens


        let setAbsDoseMin = Optic.set absDoseMinLens


        let setAbsDoseMinGStand v u dv = 
            dv 
            |>  setAbsDoseMin (ValueUnit.substanceInGStandUnit v u)


        let absDoseMaxLens = DoseValue.AbsDose_ >-> MinMax.MinMax.Max_

        
        let getAbsDoseMax = Optic.get absDoseMaxLens


        let setAbsDoseMax = Optic.set absDoseMaxLens


        let setAbsDoseMaxGStand v u dv = 
            dv 
            |>  setAbsDoseMax (ValueUnit.substanceInGStandUnit v u)

        
        let getNormDosePerKg = Optic.get DoseValue.NormDosePerKg_


        let setNormDosePerKg = Optic.set DoseValue.NormDosePerKg_


        let normDosePerKgMinLens = DoseValue.NormDosePerKg_ >-> MinMax.MinMax.Min_

        
        let getNormDosePerKgMin = Optic.get normDosePerKgMinLens


        let setNormDosePerKgMin = Optic.set normDosePerKgMinLens


        let setNormDosePerKgMinGStand v u dv = 
            dv 
            |>  setNormDosePerKgMin (ValueUnit.substanceInGStandUnit v u)


        let normDosePerKgMaxLens = DoseValue.NormDosePerKg_ >-> MinMax.MinMax.Max_

        
        let getNormDosePerKgMax = Optic.get normDosePerKgMaxLens


        let setNormDosePerKgMax = Optic.set normDosePerKgMaxLens


        let setNormDosePerKgMaxGStand v u dv = 
            dv 
            |>  setNormDosePerKgMax (ValueUnit.substanceInGStandUnit v u)

        
        let getAbsDosePerKg = Optic.get DoseValue.AbsDosePerKg_


        let setAbsDosePerKg = Optic.set DoseValue.AbsDosePerKg_


        let absDosePerKgMinLens = DoseValue.AbsDosePerKg_ >-> MinMax.MinMax.Min_

        
        let getAbsDosePerKgMin = Optic.get absDosePerKgMinLens


        let setAbsDosePerKgMin = Optic.set absDosePerKgMinLens


        let setAbsDosePerKgMinGStand v u dv = 
            dv 
            |>  setAbsDosePerKgMin (ValueUnit.substanceInGStandUnit v u)


        let absDosePerKgMaxLens = DoseValue.AbsDosePerKg_ >-> MinMax.MinMax.Max_

        
        let getAbsDosePerKgMax = Optic.get absDosePerKgMaxLens


        let setAbsDosePerKgMax = Optic.set absDosePerKgMaxLens


        let setAbsDosePerKgMaxGStand v u dv = 
            dv 
            |>  setAbsDosePerKgMax (ValueUnit.substanceInGStandUnit v u)

        
        let getNormDosePerM2 = Optic.get DoseValue.NormDosePerM2_


        let setNormDosePerM2 = Optic.set DoseValue.NormDosePerM2_


        let normDosePerM2MinLens = DoseValue.NormDosePerM2_ >-> MinMax.MinMax.Min_

        
        let getNormDosePerM2Min = Optic.get normDosePerM2MinLens


        let setNormDosePerM2Min = Optic.set normDosePerM2MinLens


        let setNormDosePerM2MinGStand v u dv = 
            dv 
            |>  setNormDosePerM2Min (ValueUnit.substanceInGStandUnit v u)


        let normDosePerM2MaxLens = DoseValue.NormDosePerM2_ >-> MinMax.MinMax.Max_

        
        let getNormDosePerM2Max = Optic.get normDosePerM2MaxLens


        let setNormDosePerM2Max = Optic.set normDosePerM2MaxLens


        let setNormDosePerM2MaxGStand v u dv = 
            dv 
            |>  setNormDosePerM2Max (ValueUnit.substanceInGStandUnit v u)

        
        let getAbsDosePerM2 = Optic.get DoseValue.AbsDosePerM2_


        let setAbsDosePerM2 = Optic.set DoseValue.AbsDosePerM2_


        let absDosePerM2MinLens = DoseValue.AbsDosePerM2_ >-> MinMax.MinMax.Min_

        
        let getAbsDosePerM2Min = Optic.get absDosePerM2MinLens


        let setAbsDosePerM2Min = Optic.set absDosePerM2MinLens


        let setAbsDosePerM2MinGStand v u dv = 
            dv 
            |>  setAbsDosePerM2Min (ValueUnit.substanceInGStandUnit v u)


        let absDosePerM2MaxLens = DoseValue.AbsDosePerM2_ >-> MinMax.MinMax.Max_

        
        let getAbsDosePerM2Max = Optic.get absDosePerM2MaxLens


        let setAbsDosePerM2Max = Optic.set absDosePerM2MaxLens


        let setAbsDosePerM2MaxGStand v u dv = 
            dv 
            |>  setAbsDosePerM2Max (ValueUnit.substanceInGStandUnit v u)

        
        type Dose with

            static member PerTime_ =
                (fun d -> if d.PerTime |> Option.isSome then d.PerTime |> Option.get else emptyDoseValue), 
                (fun pt d -> { d with PerTime = Some pt })

            static member PerPeriod_ =
                (fun d -> d.PerPeriod), (fun pp d -> { d with PerPeriod = pp })


        let getPerTime = Optic.get Dose.PerTime_


        let setPerTime = Optic.set Dose.PerTime_


        let perTimeLens = Dose.PerTime_ >-> DoseValue.NormDose_
               


    module Shape =

        type Shape =
            { 
                Name : Name
                Dose : Dose.Dose
                Unit : ShapeUnit
            }
        and ShapeUnit = CombiUnit.CombiUnit Option


        let empty = { Name = ""; Dose = Dose.empty; Unit = None }


    module GenericProduct =

        type GenericProduct = 
            {
                Id : Id
                Name : Name
            }


    module TradeProduct =

        type TradeProduct = 
            {
                Id : Id 
                Name : Name
            }


    module Substance =

        type Substance = 
            {
                Name : Name
                Dose : Dose.Dose
                Unit : CombiUnit.CombiUnit
            }


                
    /// The dose rule that applies to a 
    /// patient category for a specific 
    /// generic with a specific shape and 
    /// route. When there is a list of generic
    /// products, than the dose rule only applies 
    /// to those products. When there is a list
    /// of trade products, than the dose rule 
    /// only applies to those trade products.
    type DoseRule =
        {
            /// The generic name consisting of a substance name
            /// a combination of substance names or a generic
            /// name that identifies a substance/substances.
            Generic : Generic
            /// The ATC code that belongs to the generic
            ATC : ATC
            /// The therapy group of the generic (ATC based)
            TherapyGroup : TherapyGroup
            /// The therapeutic sub group of the generic (ATC based)
            TherapySubGroup : TherapySubGroup
            /// The indication for the dose rule 
            Indication : Indication
            /// The pharmacological shape of the products that belong to the dose rule
            Shape : Shape.Shape
            /// The route for the doserule
            Route : Route.Route
            /// The patient characteristics to which
            /// the dose rule applies
            Patient : Patient.Patient
            /// A list of generic products to which the dose rule applies to.
            /// If the list is empty, all generic products with the generic name,
            /// shape and substances belong to the dose rule.
            GenericProducts : GenericProduct.GenericProduct list
            /// A list of trade products to which the dose rule applies to.
            /// If the list is empty, all trade products with the generic name,
            /// shape and substances belong to the dose rule.
            TradeProducts : TradeProduct.TradeProduct list
            /// The list of substances that the dose rule applies to, 
            /// per substance the dose values can be defined that determine
            /// min and max dose value.
            Substances : Substance.Substance List
            /// The textual representation of the dose rule in either the G-Standard
            /// or 'Kinderformularium' or there is no text.
            Text : RuleText 
        }
    and RuleText = 
        | GSTandText of string 
        | PediatricFormText of string 
        | NoText



    let create gen atc thg tsg ind shp rte pat gps tps sbs txt = 
        {
            Generic = gen
            ATC = atc
            TherapyGroup = thg
            TherapySubGroup = tsg
            Indication = ind
            Shape = shp
            Route = rte
            Patient = pat
            GenericProducts = gps
            TradeProducts = tps
            Substances = sbs
            Text = txt
        }


    let empty =
        {
            Generic = ""
            ATC = ""
            TherapyGroup = ""
            TherapySubGroup = ""
            Indication = ""
            Shape = Shape.empty
            Route = Route.NoRoute
            Patient = Patient.empty
            GenericProducts = []
            TradeProducts = []
            Substances = []
            Text = NoText
        }


    let doseRuleText = """
Doseringsadvies voor {generic} {shape}

ATC code: {atc}
Therapie Groep : {therapygroup}
Therapie Sub Groep : {therapysubgroup}

Indicatie: {indication} 

Route: {route}

Patient : {patient}

Regels: {text}
    """


    let toString (dr : DoseRule) =
        doseRuleText
        |> String.replace "{generic}" dr.Generic 
        |> String.replace "{shape}" dr.Generic 
        |> String.replace "{atc}" dr.ATC
        |> String.replace "{therapygroup}" dr.TherapyGroup
        |> String.replace "{therapysub}" dr.TherapySubGroup
        |> String.replace "{indication}" dr.Indication
        |> String.replace "{route}" (dr.Route |> Route.toString)
        |> String.replace "{text}" <| 
            match dr.Text with
            | GSTandText txt 
            | PediatricFormText txt -> txt
            | NoText -> ""

