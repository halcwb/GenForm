namespace Informedica.GenForm.Lib


module DoseRule =

    open MathNet.Numerics
    open Aether
    open Aether.Operators

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

        open ValueUnit

        type MinMax = MinMax.MinMax<ValueUnit>


        let inRange n minmax = MinMax.inRange lte ste n minmax


        let empty = MinMax.none


        let toString minmax = 
            let toStr = ValueUnit.toStringPrec 2
            
            minmax |> MinMax.toString toStr


        let set m set v minmax =
            match m v with 
            | Some vu -> minmax |> set vu
            | None    -> minmax

        
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

        open Informedica.GenProduct.Lib.DoseRule

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
                let mms = minmax |> MinMax.toString
                if mms = "" then s 
                else
                    let s = if s = "" then s else s + "\n"
                    s + label + ": " + mms
            if p = empty then "Alle patienten"
            else 
                ""
                |> print "Leeftijd" p.Age
                |> print "Gewicht" p.Weight
                |> print "BSA" p.BSA
                |> (fun s -> 
                    s + (if s = "" then "" else "\n") +
                    (match p.Gender with
                     | Some g -> "Geslacht: " + (g |> Patient.genderToString)
                     | None -> "")
                )


        type Patient with

            static member Age_ : 
                (Patient -> MinMax.MinMax) * (MinMax.MinMax -> Patient -> Patient) =
                (fun pat -> pat.Age), (fun age pat -> { pat with Age = age } )

            static member Weight_ : 
                (Patient -> MinMax.MinMax) * (MinMax.MinMax -> Patient -> Patient) =
                (fun pat -> pat.Weight), (fun wgt pat -> { pat with Weight = wgt } )

            static member BSA_ : 
                (Patient -> MinMax.MinMax) * (MinMax.MinMax -> Patient -> Patient) =
                (fun pat -> pat.BSA), (fun bsa pat -> { pat with BSA = bsa } )

            static member GestAge_ : 
                (Patient -> MinMax.MinMax) * (MinMax.MinMax -> Patient -> Patient) =
                (fun pat -> pat.GestAge), (fun gst pat -> { pat with GestAge = gst } )

            static member Gender_ : 
                (Patient -> Option<Patient.Gender>) * (Option<Patient.Gender> -> Patient -> Patient) =
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


        // === Gender SETTERS GETTERS ==


        let getGender = Optic.get Patient.Gender_


        let setGender = Optic.set Patient.Gender_


        let isoMorphGenderMale :
            (Patient -> bool) * (bool -> Patient -> Patient) =
            (fun p -> p.Gender = Some Patient.Male ),
            (fun m p -> if m then { p with Gender = Some Patient.Male } else p)
        

        let getGenderMale = Optic.get isoMorphGenderMale


        let setGenderMale = Optic.set isoMorphGenderMale


        let isoMorphGenderFemale :
            (Patient -> bool) * (bool -> Patient -> Patient) =
            (fun p -> p.Gender = Some Patient.Female ),
            (fun m p -> if m then { p with Gender = Some Patient.Female } else p)
        

        let getGenderFemale = Optic.get isoMorphGenderFemale


        let setGenderFemale = Optic.set isoMorphGenderFemale


        let isoMorphGenderUndetermined :
            (Patient -> bool) * (bool -> Patient -> Patient) =
            (fun p -> p.Gender = Some Patient.Undetermined ),
            (fun m p -> if m then { p with Gender = Some Patient.Undetermined } else p)
        

        let getGenderUndetermined = Optic.get isoMorphGenderUndetermined


        let setGenderUndetermined = Optic.set isoMorphGenderUndetermined



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
        and TimePeriod = ValueUnit.Unit


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
                     ValueUnit.Units.fromString u)
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


        let perTimeNormDoseLens = Dose.PerTime_ >-> DoseValue.NormDose_


        let getPerTimeNormDose = Optic.get perTimeNormDoseLens


        let setPerTimeNormDose = Optic.set perTimeNormDoseLens


        let perTimeNormDoseMinLens = Dose.PerTime_ >-> normDoseMinLens


        let getPertimeNormDoseMin = Optic.get perTimeNormDoseMinLens


        let setPerTimeNormDoseMin = Optic.set perTimeNormDoseMinLens


        let perTimeNormDoseMaxLens = Dose.PerTime_ >-> normDoseMaxLens


        let getPertimeNormDoseMax = Optic.get perTimeNormDoseMaxLens


        let setPerTimeNormDoseMax = Optic.set perTimeNormDoseMaxLens
               

        let perTimeAbsDoseLens = Dose.PerTime_ >-> DoseValue.AbsDose_


        let getPerTimeAbsDose = Optic.get perTimeAbsDoseLens


        let setPerTimeAbsDose = Optic.set perTimeAbsDoseLens


        let perTimeAbsDoseMinLens = Dose.PerTime_ >-> absDoseMinLens


        let getPertimeAbsDoseMin = Optic.get perTimeAbsDoseMinLens


        let setPerTimeAbsDoseMin = Optic.set perTimeAbsDoseMinLens


        let perTimeAbsDoseMaxLens = Dose.PerTime_ >-> absDoseMaxLens


        let getPertimeAbsDoseMax = Optic.get perTimeAbsDoseMaxLens


        let setPerTimeAbsDoseMax = Optic.set perTimeAbsDoseMaxLens


        let perTimeNormDosePerKgLens = Dose.PerTime_ >-> DoseValue.NormDosePerKg_


        let getPerTimeNormDosePerKg = Optic.get perTimeNormDosePerKgLens


        let setPerTimeNormDosePerKg = Optic.set perTimeNormDosePerKgLens


        let perTimeNormDosePerKgMinLens = Dose.PerTime_ >-> normDosePerKgMinLens


        let getPertimeNormDosePerKgMin = Optic.get perTimeNormDosePerKgMinLens


        let setPerTimeNormDosePerKgMin = Optic.set perTimeNormDosePerKgMinLens


        let perTimeNormDosePerKgMaxLens = Dose.PerTime_ >-> normDosePerKgMaxLens


        let getPertimeNormDosePerKgMax = Optic.get perTimeNormDosePerKgMaxLens


        let setPerTimeNormDosePerKgMax = Optic.set perTimeNormDosePerKgMaxLens
               

        let perTimeAbsDosePerKgLens = Dose.PerTime_ >-> DoseValue.AbsDosePerKg_


        let getPerTimeAbsDosePerKg = Optic.get perTimeAbsDosePerKgLens


        let setPerTimeAbsDosePerKg = Optic.set perTimeAbsDosePerKgLens


        let perTimeAbsDosePerKgMinLens = Dose.PerTime_ >-> absDosePerKgMinLens


        let getPertimeAbsDosePerKgMin = Optic.get perTimeAbsDosePerKgMinLens


        let setPerTimeAbsDosePerKgMin = Optic.set perTimeAbsDosePerKgMinLens


        let perTimeAbsDosePerKgMaxLens = Dose.PerTime_ >-> absDosePerKgMaxLens


        let getPertimeAbsDosePerKgMax = Optic.get perTimeAbsDosePerKgMaxLens


        let setPerTimeAbsDosePerKgMax = Optic.set perTimeAbsDosePerKgMaxLens


        let perTimeNormDosePerM2Lens = Dose.PerTime_ >-> DoseValue.NormDosePerM2_


        let getPerTimeNormDosePerM2 = Optic.get perTimeNormDosePerM2Lens


        let setPerTimeNormDosePerM2 = Optic.set perTimeNormDosePerM2Lens


        let perTimeNormDosePerM2MinLens = Dose.PerTime_ >-> normDosePerM2MinLens


        let getPertimeNormDosePerM2Min = Optic.get perTimeNormDosePerM2MinLens


        let setPerTimeNormDosePerM2Min = Optic.set perTimeNormDosePerM2MinLens


        let perTimeNormDosePerM2MaxLens = Dose.PerTime_ >-> normDosePerM2MaxLens


        let getPertimeNormDosePerM2Max = Optic.get perTimeNormDosePerM2MaxLens


        let setPerTimeNormDosePerM2Max = Optic.set perTimeNormDosePerM2MaxLens
               

        let perTimeAbsDosePerM2Lens = Dose.PerTime_ >-> DoseValue.AbsDosePerM2_


        let getPerTimeAbsDosePerM2 = Optic.get perTimeAbsDosePerM2Lens


        let setPerTimeAbsDosePerM2 = Optic.set perTimeAbsDosePerM2Lens


        let perTimeAbsDosePerM2MinLens = Dose.PerTime_ >-> absDosePerM2MinLens


        let getPertimeAbsDosePerM2Min = Optic.get perTimeAbsDosePerM2MinLens


        let setPerTimeAbsDosePerM2Min = Optic.set perTimeAbsDosePerM2MinLens


        let perTimeAbsDosePerM2MaxLens = Dose.PerTime_ >-> absDosePerM2MaxLens


        let getPertimeAbsDosePerM2Max = Optic.get perTimeAbsDosePerM2MaxLens


        let setPerTimeAbsDosePerM2Max = Optic.set perTimeAbsDosePerM2MaxLens


    module Shape =

        type Shape =
            { 
                Name : Name
                Unit : ShapeUnit
            }
        and ShapeUnit = ValueUnit.Unit Option


        let empty = { Name = ""; Unit = None }

        type Shape with

            static member Name_ =
                (fun s -> s.Name), 
                (fun n s -> { s with Name = n })




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
                Unit : ValueUnit.Unit
            }

        let init name unit = 
            {
                Name = name
                Dose = Dose.empty
                Unit = unit
            }

        type Substance with

            static member Name_ =
                (fun s -> s.Name),
                (fun n s -> { s with Name = n })

            static member Dose_ =
                (fun s -> s.Dose),
                (fun d s -> { s with Dose = d })

            static member Unit_ =
                (fun s -> s.Unit), 
                (fun u s -> { s with Unit = u })


        let getName = Optic.get Substance.Name_


        let setName = Optic.set Substance.Name_


        let getUnit = Optic.get Substance.Unit_


        let setUnit = Optic.set Substance.Unit_


        let isoMorphGStandUnit =
            (fun s -> s |> getUnit |> ValueUnit.unitToString),
            (fun u s -> 
                match u |> ValueUnit.unitFromString Mapping.GStandMap with
                | Some u' -> s |> setUnit u'
                | None -> s)


        let getGStandUnit = Optic.get isoMorphGStandUnit


        let setGStandUnit = Optic.set isoMorphGStandUnit


        let getDose = Optic.get Substance.Dose_

        
        let setDose = Optic.set Substance.Dose_


        let perDosePerTimeLens = Substance.Dose_ >-> Dose.Dose.PerTime_


        let getDosePerTime = Optic.get perDosePerTimeLens


        let setDosePerTime = Optic.set perDosePerTimeLens


        let perDoserPerTimeNormDoseLens = Substance.Dose_ >-> Dose.perTimeNormDoseLens


        let getDosePerTimeNormDose = Optic.get perDoserPerTimeNormDoseLens

        
        let setDosePerTimeNormDose = Optic.set perDoserPerTimeNormDoseLens


        let perDosePerTimeNormDoseMinLens = Substance.Dose_ >-> Dose.perTimeNormDoseMinLens


        let getDosePerTimeNormDoseMin = Optic.get perDosePerTimeNormDoseMinLens


        let setDosePerTimeNormDoseMin = Optic.set perDosePerTimeNormDoseMinLens


        let perDosePerTimeNormDoseMaxLens = Substance.Dose_ >-> Dose.perTimeNormDoseMaxLens


        let getDosePerTimeNormDoseMax = Optic.get perDosePerTimeNormDoseMaxLens


        let setDosePerTimeNormDoseMax = Optic.set perDosePerTimeNormDoseMaxLens


        let perDoserPerTimeAbsDoseLens = Substance.Dose_ >-> Dose.perTimeAbsDoseLens


        let getDosePerTimeAbsDose = Optic.get perDoserPerTimeAbsDoseLens

        
        let setDosePerTimeAbsDose = Optic.set perDoserPerTimeAbsDoseLens


        let perDosePerTimeAbsDoseMinLens = Substance.Dose_ >-> Dose.perTimeAbsDoseMinLens


        let getDosePerTimeAbsDoseMin = Optic.get perDosePerTimeAbsDoseMinLens


        let setDosePerTimeAbsDoseMin = Optic.set perDosePerTimeAbsDoseMinLens


        let perDosePerTimeAbsDoseMaxLens = Substance.Dose_ >-> Dose.perTimeAbsDoseMaxLens


        let getDosePerTimeAbsDoseMax = Optic.get perDosePerTimeAbsDoseMaxLens


        let setDosePerTimeAbsDoseMax = Optic.set perDosePerTimeAbsDoseMaxLens
 

        let perDoserPerTimeNormDosePerKgLens = Substance.Dose_ >-> Dose.perTimeNormDosePerKgLens


        let getDosePerTimeNormDosePerKg = Optic.get perDoserPerTimeNormDosePerKgLens

        
        let setDosePerTimeNormDosePerKg = Optic.set perDoserPerTimeNormDosePerKgLens


        let perDosePerTimeNormDosePerKgMinLens = Substance.Dose_ >-> Dose.perTimeNormDosePerKgMinLens


        let getDosePerTimeNormDosePerKgMin = Optic.get perDosePerTimeNormDosePerKgMinLens


        let setDosePerTimeNormDosePerKgMin = Optic.set perDosePerTimeNormDosePerKgMinLens


        let perDosePerTimeNormDosePerKgMaxLens = Substance.Dose_ >-> Dose.perTimeNormDosePerKgMaxLens


        let getDosePerTimeNormDosePerKgMax = Optic.get perDosePerTimeNormDosePerKgMaxLens


        let setDosePerTimeNormDosePerKgMax = Optic.set perDosePerTimeNormDosePerKgMaxLens


        let perDoserPerTimeAbsDosePerKgLens = Substance.Dose_ >-> Dose.perTimeAbsDosePerKgLens


        let getDosePerTimeAbsDosePerKg = Optic.get perDoserPerTimeAbsDosePerKgLens

        
        let setDosePerTimeAbsDosePerKg = Optic.set perDoserPerTimeAbsDosePerKgLens


        let perDosePerTimeAbsDosePerKgMinLens = Substance.Dose_ >-> Dose.perTimeAbsDosePerKgMinLens


        let getDosePerTimeAbsDosePerKgMin = Optic.get perDosePerTimeAbsDosePerKgMinLens


        let setDosePerTimeAbsDosePerKgMin = Optic.set perDosePerTimeAbsDosePerKgMinLens


        let perDosePerTimeAbsDosePerKgMaxLens = Substance.Dose_ >-> Dose.perTimeAbsDosePerKgMaxLens


        let getDosePerTimeAbsDosePerKgMax = Optic.get perDosePerTimeAbsDosePerKgMaxLens


        let setDosePerTimeAbsDosePerKgMax = Optic.set perDosePerTimeAbsDosePerKgMaxLens


        let perDoserPerTimeNormDosePerM2Lens = Substance.Dose_ >-> Dose.perTimeNormDosePerM2Lens


        let getDosePerTimeNormDosePerM2 = Optic.get perDoserPerTimeNormDosePerM2Lens

        
        let setDosePerTimeNormDosePerM2 = Optic.set perDoserPerTimeNormDosePerM2Lens


        let perDosePerTimeNormDosePerM2MinLens = Substance.Dose_ >-> Dose.perTimeNormDosePerM2MinLens


        let getDosePerTimeNormDosePerM2Min = Optic.get perDosePerTimeNormDosePerM2MinLens


        let setDosePerTimeNormDosePerM2Min = Optic.set perDosePerTimeNormDosePerM2MinLens

        // ToDo copy to the rest 
        let setDosePerTimeNormDosePerM2MinGStand v s =
            s |> setDosePerTimeNormDosePerM2Min ((v, s |> getGStandUnit) |> Some)


        let perDosePerTimeNormDosePerM2MaxLens = Substance.Dose_ >-> Dose.perTimeNormDosePerM2MaxLens


        let getDosePerTimeNormDosePerM2Max = Optic.get perDosePerTimeNormDosePerM2MaxLens


        let setDosePerTimeNormDosePerM2Max = Optic.set perDosePerTimeNormDosePerM2MaxLens


        let setDosePerTimeNormDosePerM2MaxGStand v s =
            s |> setDosePerTimeNormDosePerM2Max ((v, s |> getGStandUnit) |> Some)


        let perDoserPerTimeAbsDosePerM2Lens = Substance.Dose_ >-> Dose.perTimeAbsDosePerM2Lens


        let getDosePerTimeAbsDosePerM2 = Optic.get perDoserPerTimeAbsDosePerM2Lens

        
        let setDosePerTimeAbsDosePerM2 = Optic.set perDoserPerTimeAbsDosePerM2Lens


        let perDosePerTimeAbsDosePerM2MinLens = Substance.Dose_ >-> Dose.perTimeAbsDosePerM2MinLens


        let getDosePerTimeAbsDosePerM2Min = Optic.get perDosePerTimeAbsDosePerM2MinLens


        let setDosePerTimeAbsDosePerM2Min = Optic.set perDosePerTimeAbsDosePerM2MinLens


        let setDosePerTimeAbsDosePerM2MinGStand v s =
            s |> setDosePerTimeAbsDosePerM2Min ((v, s |> getGStandUnit) |> Some)


        let perDosePerTimeAbsDosePerM2MaxLens = Substance.Dose_ >-> Dose.perTimeAbsDosePerM2MaxLens


        let getDosePerTimeAbsDosePerM2Max = Optic.get perDosePerTimeAbsDosePerM2MaxLens


        let setDosePerTimeAbsDosePerM2Max = Optic.set perDosePerTimeAbsDosePerM2MaxLens


        let setDosePerTimeAbsDosePerM2MaxGStand v s =
            s |> setDosePerTimeAbsDosePerM2Max ((v, s |> getGStandUnit) |> Some)
            

        let printDose (substs : Substance List) =
            substs
            |> List.fold (fun state s -> 
                let n = s |> getName
                if state = "" then n
                else
                    state + "\n" + n
            ) ""



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


    type DoseRule with

        static member Generic_ =
            (fun dr -> dr.Generic),
            (fun s dr -> { dr with Generic = s })

        static member Shape_ =
            (fun dr -> dr.Shape),
            (fun s dr -> { dr with Shape = s })

        static member Route_ =
            (fun dr -> dr.Route),
            (fun r dr -> { dr with Route = r })

        static member ATC_ =
            (fun dr -> dr.ATC),
            (fun s dr -> { dr with ATC = s })

        static member TherapyGroup_ =
            (fun dr -> dr.TherapyGroup),
            (fun s dr -> { dr with TherapyGroup = s })

        static member TherapySubGroup_ =
            (fun dr -> dr.TherapySubGroup),
            (fun s dr -> { dr with TherapySubGroup = s })

        static member Indication_ =
            (fun dr -> dr.Indication),
            (fun s dr -> { dr with Indication = s })

        static member Patient_ =
            (fun dr -> dr.Patient) ,
            (fun p dr -> { dr with Patient = p })


    // === Generic SETTERS GETTERS ==
 

    let getGeneric = Optic.get DoseRule.Generic_


    let setGeneric = Optic.set DoseRule.Generic_


    // === Shape SETTERS GETTERS ==
 

    let getShape = Optic.get DoseRule.Shape_


    let setShape = Optic.set DoseRule.Shape_


    let shapeNameLens = DoseRule.Shape_ >-> Shape.Shape.Name_


    let getShapeName = Optic.get shapeNameLens


    let setShapeName = Optic.set shapeNameLens



    // === Route SETTERS GETTERS ==
 

    let getRoute = Optic.get DoseRule.Route_


    let setRoute = Optic.set DoseRule.Route_


    let isoMorphRouteName =
        (fun r -> r |> Route.toString),
        (fun s r -> match s |> Route.fromString with | Some r' -> r' | None -> r)


    let RouteNameLens = DoseRule.Route_ >-> isoMorphRouteName


    let getRouteName = Optic.get RouteNameLens


    let setRouteName = Optic.set RouteNameLens


    // === ATC SETTERS GETTERS ==
 

    let getATC = Optic.get DoseRule.ATC_


    let setATC = Optic.set DoseRule.ATC_


    // === TherapyGroup SETTERS GETTERS ==
 

    let getTherapyGroup = Optic.get DoseRule.TherapyGroup_


    let setTherapyGroup = Optic.set DoseRule.TherapyGroup_


    // === TherapySubGroup SETTERS GETTERS ==
 

    let getTherapySubGroup = Optic.get DoseRule.TherapySubGroup_


    let setTherapySubGroup = Optic.set DoseRule.TherapySubGroup_


    // === Indication SETTERS GETTERS ==
 

    let getIndication = Optic.get DoseRule.Indication_


    let setIndication = Optic.set DoseRule.Indication_


    // === Patient SETTERS GETTERS ==
 

    let getPatient = Optic.get DoseRule.Patient_


    let setPatient = Optic.set DoseRule.Patient_


    // === Age SETTERS GETTERS ==

    let patientAgeLens = DoseRule.Patient_ >-> Patient.Patient.Age_


    let getPatientAge = Optic.get patientAgeLens


    let setPatientAge = Optic.set patientAgeLens


    let patientMinAgeLens = DoseRule.Patient_ >-> Patient.minAgeLens
    

    let getPatientMinAge = Optic.get patientMinAgeLens
 

    let setPatientMinAge = Optic.set patientMinAgeLens


    let patientMaxAgeLens = DoseRule.Patient_ >-> Patient.maxAgeLens


    let getPatientMaxAge = Optic.get patientMaxAgeLens
 

    let setPatientMaxAge = Optic.set patientMaxAgeLens


    // === Weight SETTERS GETTERS ==

    let patientWeightLens = DoseRule.Patient_ >-> Patient.Patient.Weight_


    let getPatientWeight = Optic.get patientWeightLens


    let setPatientWeight = Optic.set patientWeightLens


    let patientMinWeightLens = DoseRule.Patient_ >-> Patient.minWeightLens
    

    let getPatientMinWeight = Optic.get patientMinWeightLens
 

    let setPatientMinWeight = Optic.set patientMinWeightLens


    let patientMaxWeightLens = DoseRule.Patient_ >-> Patient.maxWeightLens


    let getPatientMaxWeight = Optic.get patientMaxWeightLens
 

    let setPatientMaxWeight = Optic.set patientMaxWeightLens


    // === BSA SETTERS GETTERS ==

    let patientBSALens = DoseRule.Patient_ >-> Patient.Patient.BSA_


    let getPatientBSA = Optic.get patientBSALens


    let setPatientBSA = Optic.set patientBSALens


    let patientMinBSALens = DoseRule.Patient_ >-> Patient.minBSALens
    

    let getPatientMinBSA = Optic.get patientMinBSALens
 

    let setPatientMinBSA = Optic.set patientMinBSALens


    let patientMaxBSALens = DoseRule.Patient_ >-> Patient.maxBSALens


    let getPatientMaxBSA = Optic.get patientMaxBSALens
 

    let setPatientMaxBSA = Optic.set patientMaxBSALens


    // === GestAge SETTERS GETTERS ==

    let patientGestAgeLens = DoseRule.Patient_ >-> Patient.Patient.GestAge_


    let getPatientGestAge = Optic.get patientGestAgeLens


    let setPatientGestAge = Optic.set patientGestAgeLens


    let patientMinGestAgeLens = DoseRule.Patient_ >-> Patient.minGestAgeLens
    

    let getPatientMinGestAge = Optic.get patientMinGestAgeLens
 

    let setPatientMinGestAge = Optic.set patientMinGestAgeLens


    let patientMaxGestAgeLens = DoseRule.Patient_ >-> Patient.maxGestAgeLens


    let getPatientMaxGestAge = Optic.get patientMaxGestAgeLens
 

    let setPatientMaxGestAge = Optic.set patientMaxGestAgeLens


    // === Gender SETTERS GETTERS ==

    let patientGenderLens = DoseRule.Patient_ >-> Patient.Patient.Gender_


    let getPatientGender = Optic.get patientGenderLens


    let setPatientGender = Optic.set patientGenderLens


    let patientMaleGenderLens = DoseRule.Patient_ >-> Patient.isoMorphGenderMale
    

    let getPatientMaleGender = Optic.get patientMaleGenderLens
 

    let setPatientMaleGender = Optic.set patientMaleGenderLens


    let patientFemaleGenderLens = DoseRule.Patient_ >-> Patient.isoMorphGenderFemale


    let getPatientFemaleGender = Optic.get patientFemaleGenderLens
 

    let setPatientFemaleGender = Optic.set patientFemaleGenderLens


    let patientUndeterminedGenderLens = DoseRule.Patient_ >-> Patient.isoMorphGenderUndetermined


    let getPatientUndeterminedGender = Optic.get patientUndeterminedGenderLens
 

    let setPatientUndeterminedGender = Optic.set patientUndeterminedGenderLens


    let addSubstance subst (dr : DoseRule) = 
        { dr with Substances = dr.Substances |> List.append [subst] }


    let getSubstances (dr : DoseRule) = dr.Substances


    let doseRuleText = """
Doseringsadvies voor {generic} {shape}

ATC code: {atc}
Therapie Groep : {therapygroup}
Therapie Sub Groep : {therapysubgroup}

Indicatie: {indication} 

Route: {route}

Patient:
{patient}

Doseringen:
{doseringen}

Regels: 
{text}
    """


    let toString (dr : DoseRule) =
        doseRuleText
        |> String.replace "{generic}" dr.Generic 
        |> String.replace "{shape}" (dr |> getShapeName)
        |> String.replace "{route}" (dr |> getRouteName)
        |> String.replace "{atc}" dr.ATC
        |> String.replace "{therapygroup}" dr.TherapyGroup
        |> String.replace "{therapysubgroup}" dr.TherapySubGroup
        |> String.replace "{indication}" dr.Indication
        |> String.replace "{route}" (dr.Route |> Route.toString)
        |> String.replace "{patient}" (dr.Patient |> Patient.toString)
        |> String.replace "{doseringen}" (dr.Substances |> Substance.printDose)
        |> String.replace "{text}" ( 
            match dr.Text with
            | GSTandText txt 
            | PediatricFormText txt -> txt
            | NoText -> "")
