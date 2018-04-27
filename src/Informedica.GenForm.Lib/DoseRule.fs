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


        type MinMax = MinMax.MinMax<ValueUnit.ValueUnit>


        let empty = MinMax.none


        let toString minmax = 
            let toStr = ValueUnit.toLangString Unit.Units.Dutch 2
            
            minmax |> MinMax.toString toStr


        let set m set v minmax =
            match m v with 
            | Some vu -> minmax |> set vu
            | None -> MinMax.none

        
        let inline get get minmax = 
            minmax
            |> get
            |> Option.bind (fun vu ->
                vu
                |> ValueUnit.get
                |> fst
                |> BigRational.toFloat
                |> Some
            )

        
        let setAge = set ValueUnit.ageInMo

        
        let setAgeMin = setAge MinMax.setMin


        let setAgeMax = setAge MinMax.setMax


        let ageMinMax min max =
            empty
            |> setAgeMin min
            |> setAgeMax max


        let getAgeMin = get MinMax.getMin


        let getAgeMax = get MinMax.getMax


        let getAgeMinMax minmax = 
            minmax |> getAgeMin ,
            minmax |> getAgeMax

        
        let setWeight = set ValueUnit.weightInKg

        
        let setWeightMin = setWeight MinMax.setMin


        let setWeightMax = setWeight MinMax.setMax


        let weightMinMax min max =
            empty
            |> setWeightMin min
            |> setWeightMax max

        
        let getWeightMin = get MinMax.getMin


        let getWeightMax = get MinMax.getMax


        let getWeightMinMax minmax = 
            minmax |> getWeightMin ,
            minmax |> getWeightMax

        
        let setBSA = set ValueUnit.bsaInM2

        
        let setBSAMin = setBSA MinMax.setMin


        let setBSAMax = setBSA MinMax.setMax


        let setBSAMinMax min max =
            empty
            |> setBSAMin min
            |> setBSAMax max

        
        let getBSAMin = get MinMax.getMin


        let getBSAMax = get MinMax.getMax


        let getBSAMinMax minmax = 
            minmax |> getBSAMin ,
            minmax |> getBSAMax

        
        let setGestAge = set ValueUnit.gestAgeInDaysAndWeeks

        
        let setGestAgeMin = setGestAge MinMax.setMin


        let setGestAgeMax = setGestAge MinMax.setMax


        let setGestAgeMinMax min max =
            empty
            |> setGestAgeMin min
            |> setGestAgeMax max

        
        let getGestAgeMin = get MinMax.getMin


        let getGestAgeMax = get MinMax.getMax


        let getGestAgeMinMax minmax = 
            minmax |> getGestAgeMin ,
            minmax |> getGestAgeMax


        let setSubstance set vu minmax =
            match vu with
            | Some (v, u) ->
                match ValueUnit.substanceInGStandUnit v u with 
                | Some vu -> minmax |> set vu
                | None -> MinMax.none
            | None -> MinMax.none


        let setSubstanceMin = setSubstance MinMax.setMin


        let setSubstanceMax = setSubstance MinMax.setMax


        let setSubstanceMinMax min max =
            empty
            |> setSubstanceMin min
            |> setSubstanceMax max


        let getSubstance get minmax =
            minmax
            |> get
            |> Option.bind (fun vu ->
                vu
                |> ValueUnit.getSubstanceInGStandUnit
                |> Some
            )


        let getSubstanceMin = getSubstance MinMax.getMin


        let getSubstanceMax = getSubstance MinMax.getMax


        let getSubstanceMinMax minmax =
            minmax |> getSubstanceMin ,
            minmax |> getSubstanceMax


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
                label + ": " + (minmax |> MinMax.toString)
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


        let isoMorphAgeMin = 
            (fun minmax -> minmax |> MinMax.getAgeMin), 
            (fun min minmax -> minmax |> MinMax.setAgeMin min) 


        let minAgeLens = Patient.Age_ >-> isoMorphAgeMin


        let getMinAge = Optic.get minAgeLens
 

        let setMinAge = Optic.set minAgeLens


        let isoMorphAgeMax = 
            (fun minmax -> minmax |> MinMax.getAgeMax), 
            (fun max minmax -> minmax |> MinMax.setAgeMax max) 


        let maxAgeLens = Patient.Age_ >-> isoMorphAgeMax


        let getMaxAge = Optic.get maxAgeLens
 

        let setMaxAge = Optic.set maxAgeLens



        // === Weight SETTERS GETTERS ==

        let getWeight = Optic.get Patient.Weight_


        let setWeight = Optic.set Patient.Weight_


        let isoMorphWeightMin =
            (fun minmax -> minmax |> MinMax.getWeightMin) ,
            (fun min minmax -> minmax |> MinMax.setWeightMin min)
        

        let minWeightLens = Patient.Weight_ >-> isoMorphWeightMin


        let getMinWeight = Optic.get minWeightLens


        let setMinWeight = Optic.set minWeightLens


        let isoMorphWeightMax =
            (fun minmax -> minmax |> MinMax.getWeightMax) ,
            (fun max minmax -> minmax |> MinMax.setWeightMax max)
 

        let maxWeightLens = Patient.Weight_ >-> isoMorphWeightMax


        let getMaxWeight = Optic.get maxWeightLens
 

        let setMaxWeight = Optic.set maxWeightLens


        // === BSA SETTERS GETTERS ==

        let getBSA = Optic.get Patient.BSA_


        let setBSA = Optic.set Patient.BSA_


        let isoMorphBSAMin =
            (fun minmax -> minmax |> MinMax.getBSAMin) ,
            (fun min minmax -> minmax |> MinMax.setBSAMin min)
        

        let minBSALens = Patient.BSA_ >-> isoMorphBSAMin


        let getMinBSA = Optic.get minBSALens


        let setMinBSA = Optic.set minBSALens


        let isoMorphBSAMax =
            (fun minmax -> minmax |> MinMax.getBSAMax) ,
            (fun max minmax -> minmax |> MinMax.setBSAMax max)
 

        let maxBSALens = Patient.BSA_ >-> isoMorphBSAMax


        let getMaxBSA = Optic.get maxBSALens
 

        let setMaxBSA = Optic.set maxBSALens



        // === GestAge SETTERS GETTERS ==

        let getGestAge = Optic.get Patient.GestAge_


        let setGestAge = Optic.set Patient.GestAge_


        let isoMorphGestAgeMin =
            (fun minmax -> 
                minmax 
                |> MinMax.getGestAgeMin
                |> Option.bind (fun f ->
                    let wks = f / 7.
                    let days = f - wks
                    (wks, days) |> Some
                )
            ) ,
            (fun min minmax -> minmax |> MinMax.setGestAgeMin min)
        

        let minGestAgeLens = Patient.GestAge_ >-> isoMorphGestAgeMin


        let getMinGestAge = Optic.get minGestAgeLens


        let setMinGestAge = Optic.set minGestAgeLens


        let isoMorphGestAgeMax =
            (fun minmax -> 
                minmax 
                |> MinMax.getGestAgeMax
                |> Option.bind (fun f ->
                    let wks = f / 7.
                    let days = f - wks
                    (wks, days) |> Some
                )
            ) ,
            (fun max minmax -> minmax |> MinMax.setGestAgeMax max)
 

        let maxGestAgeLens = Patient.GestAge_ >-> isoMorphGestAgeMax


        let getMaxGestAge = Optic.get maxGestAgeLens
 

        let setMaxGestAge = Optic.set maxGestAgeLens


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


        let isoMorphDoseMin = 
            (fun minmax -> minmax |> MinMax.getSubstanceMin) ,
            (fun min minmax -> minmax |> MinMax.setSubstanceMin min)


        let normDoseMinLens = DoseValue.NormDose_ >-> isoMorphDoseMin

        
        let getNormDoseMin = Optic.get normDoseMinLens


        let setNormDoseMin = Optic.set normDoseMinLens


        let isoMorphDoseMax = 
            (fun minmax -> minmax |> MinMax.getSubstanceMax) ,
            (fun max minmax -> minmax |> MinMax.setSubstanceMax max)


        let normDoseMaxLens = DoseValue.NormDose_ >-> isoMorphDoseMax

        
        let getNormDoseMax = Optic.get normDoseMaxLens


        let setNormDoseMax = Optic.set normDoseMaxLens


        let getAbsDose = Optic.get DoseValue.AbsDose_


        let setAbsDose = Optic.set DoseValue.AbsDose_


        let absDoseMinLens = DoseValue.AbsDose_ >-> isoMorphDoseMin

        
        let getAbsDoseMin = Optic.get absDoseMinLens


        let setAbsDoseMin = Optic.set absDoseMinLens


        let absDoseMaxLens = DoseValue.AbsDose_ >-> isoMorphDoseMax

        
        let getAbsDoseMax = Optic.get absDoseMaxLens


        let setAbsDoseMax = Optic.set absDoseMaxLens


        let getNormDosePerKg = Optic.get DoseValue.NormDosePerKg_


        let setNormDosePerKg = Optic.set DoseValue.NormDosePerKg_


        let normDosePerKgMinLens = DoseValue.NormDosePerKg_ >-> isoMorphDoseMin

        
        let getNormDosePerKgMin = Optic.get normDosePerKgMinLens


        let setNormDosePerKgMin = Optic.set normDosePerKgMinLens


        let normDosePerKgMaxLens = DoseValue.NormDosePerKg_ >-> isoMorphDoseMax

        
        let getNormDosePerKgMax = Optic.get normDosePerKgMaxLens


        let setNormDosePerKgMax = Optic.set normDosePerKgMaxLens


        let getAbsDosePerKg = Optic.get DoseValue.AbsDosePerKg_


        let setAbsDosePerKg = Optic.set DoseValue.AbsDosePerKg_


        let absDosePerKgMinLens = DoseValue.AbsDosePerKg_ >-> isoMorphDoseMin

        
        let getAbsDosePerKgMin = Optic.get absDosePerKgMinLens


        let setAbsDosePerKgMin = Optic.set absDosePerKgMinLens


        let absDosePerKgMaxLens = DoseValue.AbsDosePerKg_ >-> isoMorphDoseMax

        
        let getAbsDosePerKgMax = Optic.get absDosePerKgMaxLens


        let setAbsDosePerKgMax = Optic.set absDosePerKgMaxLens

        
        let getNormDosePerM2 = Optic.get DoseValue.NormDosePerM2_


        let setNormDosePerM2 = Optic.set DoseValue.NormDosePerM2_


        let normDosePerM2MinLens = DoseValue.NormDosePerM2_ >-> isoMorphDoseMin

        
        let getNormDosePerM2Min = Optic.get normDosePerM2MinLens


        let setNormDosePerM2Min = Optic.set normDosePerM2MinLens


        let normDosePerM2MaxLens = DoseValue.NormDosePerM2_ >-> isoMorphDoseMax

        
        let getNormDosePerM2Max = Optic.get normDosePerM2MaxLens


        let setNormDosePerM2Max = Optic.set normDosePerM2MaxLens

        
        let getAbsDosePerM2 = Optic.get DoseValue.AbsDosePerM2_


        let setAbsDosePerM2 = Optic.set DoseValue.AbsDosePerM2_


        let absDosePerM2MinLens = DoseValue.AbsDosePerM2_ >-> isoMorphDoseMin

        
        let getAbsDosePerM2Min = Optic.get absDosePerM2MinLens


        let setAbsDosePerM2Min = Optic.set absDosePerM2MinLens


        let absDosePerM2MaxLens = DoseValue.AbsDosePerM2_ >-> isoMorphDoseMax

        
        let getAbsDosePerM2Max = Optic.get absDosePerM2MaxLens


        let setAbsDosePerM2Max = Optic.set absDosePerM2MaxLens

        
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

