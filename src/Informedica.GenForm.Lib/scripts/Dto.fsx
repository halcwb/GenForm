
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





module Dto =

    open Aether  

    open Informedica.GenForm.Lib

    open System
    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenUnits.Lib    

    open GStand
    open Informedica.GenUtils.Lib.BCL

    module Dosage = DoseRule.Dosage
    
    module RF = Informedica.GenProduct.Lib.RuleFinder 
    module DR = Informedica.GenProduct.Lib.DoseRule
    module GPP = Informedica.GenProduct.Lib.GenPresProduct
    module FP = Informedica.GenProduct.Lib.FilePath
    module ATC = Informedica.GenProduct.Lib.ATCGroup


    [<CLIMutable>]
    type Dto = 
        {
            BirthYear : int
            BirthMonth : int
            BirthDay : int
            WeightKg : float
            BirthWeightGram : float
            LengthCm : float
            Gender : string
            GestAgeWeeks : int
            GestAgeDays : int
            GPK : string
            ATC : string
            TherapyGroup : string
            TherapySubGroup : string
            Generic : string
            TradeProduct : string
            Shape : string
            Label : string
            Concentration : float
            ConcentrationUnit : string
            Multiple : float
            MultipleUnit : string
            Route : string
            Indication : string
            Rules : Rule []
        }
    and Rule =
        {
            Frequency : string

            NormDose : float
            MinDose : float
            MaxDose : float
            MaxTotal : float
            MaxPerDose : float

            NormDosePerKg : float
            MinDosePerKg : float
            MaxDosePerKg : float
            MaxTotalPerKg : float
            MaxPerDosePerKg : float

            NormDosePerM2 : float
            MinDosePerM2 : float
            MaxDosePerM2 : float
            MaxTotalPerM2 : float
            MaxPerDosePerM2 : float

            Text : string
        }


    let dto = 
        {
            BirthYear = 0
            BirthMonth = 0
            BirthDay = 0
            WeightKg = 0.
            BirthWeightGram = 0.
            LengthCm = 0.
            Gender = ""
            GestAgeWeeks = 0
            GestAgeDays = 0
            GPK = ""
            ATC = ""
            TherapyGroup = ""
            TherapySubGroup = ""
            Generic = ""
            TradeProduct = ""
            Shape = ""
            Label = ""
            Concentration = 0.
            ConcentrationUnit = ""
            Multiple = 0.
            MultipleUnit = ""
            Route = ""
            Indication = ""
            Rules = [||]
        }

    type Mapping = FormMap | GStandMap | PedMap  | StandMap


    let mapping path m1 m2 s =
        let i1, i2 =
            match m1, m2 with
            | FormMap,   GStandMap -> 0, 1
            | GStandMap, FormMap   -> 1, 0
            | FormMap,   StandMap  -> 0, 3
            | GStandMap, StandMap  -> 1, 3
            | StandMap,  FormMap   -> 3, 0
            | StandMap,  GStandMap -> 1, 3
            | _ -> 0, 0

        let map =
            File.readAllLines path
            |> Array.skip 1
            |> Array.map (String.splitAt ';')
        if i1 = 0 && i2 = 0 || (i1 > map.Length || i2 > map.Length) then ""
        else
            map
            |> Array.filter (fun x -> x.[i1] |> String.equalsCapInsens s)
            |> Array.fold (fun acc xs ->  
                if acc = "" then xs.[i2]
                else acc + "||" + xs.[i2]
            ) ""


    let unitMapping = mapping (Environment.CurrentDirectory + "/" + FP.data + "/formulary/UnitMapping.csv")

    
    let frequencyMapping = mapping (Environment.CurrentDirectory + "/" + FP.data + "/formulary/FrequencyMapping.csv")


    let loadGenForm () =
        GPP.load false
        DR.load ()
        ATC.load ()

    
    let toDto age weight bsa gpk route =
        let gen, shp =
            match gpk |> GPP.findByGPK |> Array.toList with
            | [gpp] -> gpp.Name, gpp.Shape
            | _ -> "", ""

        let rs = GStand.createDoseRules false age weight bsa (Some gpk) gen shp route
        
        rs
        |> Seq.iter (fun r -> r |> DoseRule.toString false |> printfn "%s")

        if rs |> Seq.length <> 1 then dto
        else
            let r = rs |> Seq.head

            {
                dto with
                    ATC = r.ATC
                    TherapyGroup = r.ATCTherapyGroup
                    TherapySubGroup = r.ATCTherapySubGroup
                    Generic = r.Generic
                    TradeProduct = ""
                    Shape = ""
                    Label = ""
                    Concentration = 0.
                    ConcentrationUnit = ""
                    Multiple = 0.
                    MultipleUnit = ""
                    Route = route
            }


Dto.toDto (Some 3.) (Some 5.) None 3689 ""

