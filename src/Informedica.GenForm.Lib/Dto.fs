namespace Informedica.GenForm.Lib


module Dto =

    open System
    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open Informedica.GenProduct.Lib
    open Informedica.GenUnits.Lib
    

    // Private m_BD As Date
    // Private m_WT As Double
    // Private m_BW As Double
    // Private m_LE As Double
    // Private m_GS As String
    // Private m_GW As Integer
    // Private m_GD As Integer
    // Private m_GPK As String
    // Private m_ATC As String
    // Private m_TherapieGroep As String
    // Private m_TherapieSubgroep As String
    // Private m_Generiek As String
    // Private m_Product As String
    // Private m_Vorm As String
    // Private m_Etiket As String
    // Private m_Sterkte As Double
    // Private m_SterkteEenheid As String
    // Private m_DeelDose As Double
    // Private m_DoseEenheid As String
    // Private m_Route As String
    // Private m_Indicatie As String
    // Private m_Freq As String
    // Private m_PerDose As Boolean
    // Private m_PerKg As Boolean
    // Private m_PerM2 As Boolean
    // Private m_NormDose As Double
    // Private m_MinDose As Double
    // Private m_MaxDose As Double
    // Private m_AbsDose As Double
    // Delimiter string = ||

    // BSA = m_WT ^ 0.425 * m_LE ^ 0.725 * 0.007184


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
            Frequency : string
            PerDose : bool
            PerKg : bool
            PerM2 : bool
            NormDose : float
            MinDose : float
            MaxDose : float
            AbsMaxTotal : float
            AbsMaxPerDose : float
            Rules : string
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
            Frequency = ""
            PerDose = false
            PerKg = false
            PerM2 = false
            NormDose = 0.
            MinDose = 0.
            MaxDose = 0.
            AbsMaxTotal = 0.
            AbsMaxPerDose = 0.
            Rules = ""
        }


    let testDto = 

        let by, bm, bd =
            let n = 
                DateTime.now ()
                |> DateTime.addYears -1
            n.Year, n.Month, n.Day
            
        {
            BirthYear = by
            BirthMonth = bm
            BirthDay = bd
            WeightKg = 10.
            BirthWeightGram = 3000.
            LengthCm = 70.
            Gender = "Test"
            GestAgeWeeks = 40
            GestAgeDays = 1
            GPK = "000000"
            ATC = "Test"
            TherapyGroup = "Test"
            TherapySubGroup = "Test"
            Generic = "Test"
            TradeProduct = "Test"
            Shape = "Test"
            Label = "Test"
            Concentration = 1.
            ConcentrationUnit = "mg/ml"
            Multiple = 0.
            MultipleUnit = "mcg"
            Route = "oraal"
            Indication = "Test"
            Frequency = "2 x / 3 dagen"
            PerDose = false
            PerKg = true
            PerM2 = false
            NormDose = 10.
            MinDose = 5.
            MaxDose = 20.
            AbsMaxTotal = 500.
            AbsMaxPerDose = 50.
            Rules = "Test"
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

        File.readAllLines path
        |> Array.skip 1
        |> Array.map (String.splitAt ';')
        |> Array.filter (fun x -> x.[i1] |> String.equalsCapInsens s)
        |> Array.fold (fun acc xs ->  
            if acc = "" then xs.[i2]
            else acc + "||" + xs.[i2]
        ) ""


    let unitMapping = mapping (Environment.CurrentDirectory + "/" + FilePath.data + "/formulary/UnitMapping.csv")

    let frequencyMapping = mapping (Environment.CurrentDirectory + "/" + FilePath.data + "/formulary/FrequencyMapping.csv")

    let toDto (dto : Dto) (rs : RuleFinder.RuleResult) =
        // get only doses with mappable frequencies
        let doses = 
            rs.Doses
            |> Array.filter (fun d ->
                (string d.Freq.Frequency + " " + d.Freq.Time)
                |> frequencyMapping GStandMap FormMap 
                |> ((<>) "")
            ) 

        match doses with
        | [||] ->
            dto
        | _ ->

            let unit =
                doses
                |> Array.fold (fun acc d ->
                    if acc = "" then d.Unit
                    else acc
                ) ""

            // AbsMax per dose
            let absPer = 
                let dosesOnce =
                    doses
                    |> Array.filter (fun d ->
                        d.Freq.Frequency = 1.
                    )
                dosesOnce
                |> Array.map (fun d -> d.NormDose)
                |> Array.append (dosesOnce |> Array.map (fun d -> d.AbsDose))
                |> DoseRule.foldMinMax
                |> (fun minmax ->
                    match minmax.Min, minmax.Max with
                    | _, Some max -> max
                    | _ -> 0.
                )

            // normal dose
            let normDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormDose)
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, Some max when min = max -> (min, false, false)
                    | _                                 -> (0.,  false, false)
                )

            // normal dose per Kg
            let normDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormKg)
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, Some max when min = max -> (min,      true,  false)
                    | _                                 -> (normDose, perKg, perM2)
                )

            // normal dose per m2
            let normDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormM2)
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, Some max when min = max -> (min,     false,  true)
                    | _                                 -> (normDose, perKg, perM2)
                )

            // max dose
            let maxDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormDose)
                |> Array.append (doses |> Array.map (fun d -> d.AbsDose))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | None, Some max                     -> (max, false, false)
                    | Some min, Some max when min <> max -> (max, false, false)
                    | _                                  -> (0.,  perKg, perM2)
                )

            // abs max dose
            let absMax = maxDose

            // max dose per kg
            let maxDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormKg)
                |> Array.append (doses |> Array.map (fun d -> d.AbsKg))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | None, Some max                     -> (max, true,  false)
                    | Some min, Some max when min <> max -> (max, true,  false)
                    | _                                  -> (maxDose, perKg, perKg)
                )

            // max dose per m2
            let maxDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormM2)
                |> Array.append (doses |> Array.map (fun d -> d.AbsM2))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | None, Some max                     -> (max, false, true)
                    | Some min, Some max when min <> max -> (max, false, true)
                    | _                                  -> (maxDose, perKg, perM2)
                )


            // min dose
            let minDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormDose)
                |> Array.append (doses |> Array.map (fun d -> d.AbsDose))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, None                     -> (min, false, false)
                    | Some min, Some max when min <> max -> (min, false, false)
                    | _                                  -> (0.,  perKg, perM2)
                )

            // min dose per kg
            let minDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormKg)
                |> Array.append (doses |> Array.map (fun d -> d.AbsKg))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, None                     -> (min, true, false)
                    | Some min, Some max when min <> max -> (min, true, false)
                    | _                                  -> (minDose, perKg, perM2)
                )

            // min dose per m2
            let minDose, perKg, perM2 =
                doses
                |> Array.map (fun d -> d.NormM2)
                |> Array.append (doses |> Array.map (fun d -> d.AbsM2))
                |> DoseRule.foldMinMax
                |> (fun minmax -> 
                    match minmax.Min, minmax.Max with
                    | Some min, None                     -> (min, false, true)
                    | Some min, Some max when min <> max -> (min, false, true)
                    | _                                  -> (minDose,  perKg, perM2)
                )

            let convertTo u1 u2 v =
                let u1, u2 = 
                    u1 |> unitMapping FormMap   StandMap,
                    u2 |> unitMapping GStandMap StandMap
                if u2 = "" || u1 = "" || u1 = u2 then v
                else
                    let br = BigRational.fromFloat v
                    BigRational.toString br + " " + u2 
                    |> Api.convert ((BigRational.toString 1N) + " " + u1)
                    |> (ValueUnit.fromString >> ValueUnit.get >> fst >> BigRational.toFloat)


            { dto with
                Frequency = 
                    doses
                    |> Array.map (fun d ->
                        (string d.Freq.Frequency + " " + d.Freq.Time)
                        |> frequencyMapping GStandMap FormMap 
                    )
                    |> String.concat "||"

                // make sure that the multiple unit is mappable and the
                // same for each dose
                MultipleUnit = 
                    doses
                    |> Array.fold (fun acc d ->
                        if unitMapping GStandMap FormMap d.Unit = acc then acc
                        else ""
                    ) dto.MultipleUnit

                PerKg          = perKg 
                PerM2          = perM2 
                NormDose       = normDose |> convertTo dto.MultipleUnit unit |> Double.fixPrecision 3
                MinDose        = minDose  |> convertTo dto.MultipleUnit unit |> Double.fixPrecision 3
                MaxDose        = maxDose  |> convertTo dto.MultipleUnit unit |> Double.fixPrecision 3
                AbsMaxTotal    = absMax   |> convertTo dto.MultipleUnit unit |> Double.fixPrecision 3
                AbsMaxPerDose  = absPer   |> convertTo dto.MultipleUnit unit |> Double.fixPrecision 3
            }

        |> (fun dto' ->

            let atc =
                if dto'.ATC <> "" then dto'.ATC
                else 
                    if rs.Product.GenericProducts |> Array.isEmpty then dto'.ATC
                    else
                        rs.Product.GenericProducts
                        |> Array.fold (fun acc gp ->
                            if gp.ATC = acc then acc else dto'.ATC
                        ) rs.Product.GenericProducts.[0].ATC

            let groups = 
                atc 
                |> ATCGroup.findByATC5 
            

            { dto' with
                ATC = atc

                TherapyGroup =
                    if dto'.TherapyGroup <> "" then dto'.TherapyGroup
                    else
                        groups
                        |> Array.fold (fun acc g ->
                            if acc = "" then g.TherapeuticMainGroup else acc
                        ) ""
                        
                TherapySubGroup =
                    if dto'.TherapySubGroup <> "" then dto'.TherapySubGroup
                    else
                        groups
                        |> Array.fold (fun acc g ->
                            if acc = "" then g.TherapeuticSubGroup else acc
                        ) ""
                        
                Generic = 
                    if dto'.Generic <> "" then dto'.Generic
                    else
                        if rs.Product.DisplayName = "" then rs.Product.Name 
                        else rs.Product.DisplayName
                
                Shape = 
                    if dto'.Shape <> "" then dto'.Shape
                    else rs.Product.Shape
                        
                Label = 
                    if dto'.Label <> "" then dto'.Label
                    else 
                        rs.Product.GenericProducts
                        |> Array.fold (fun acc gp ->
                            if acc = "" then gp.Label
                            else acc
                        ) ""

                Rules = 
                    rs.DoseRules 
                    |> String.concat "||"
            }
        )

    let findRules (dto : Dto) =
        let age = 
            let dt = DateTime(dto.BirthYear, dto.BirthMonth, dto.BirthDay)
            ((DateTime.Now - dt).Days |> float) / 30.
            |> Some

        let bsa =
            if dto.LengthCm > 0. && dto.WeightKg > 0. then
                (dto.WeightKg ** 0.425) * (dto.LengthCm ** 0.725) * 0.007184
                |> Some
            else None

        let wght = 
            if dto.WeightKg > 0. then dto.WeightKg |> Some else None

        let gpk = if dto.GPK = "" then None else dto.GPK |> Int32.parse |> Some
        
        RuleFinder.createFilter age wght bsa gpk "" "" dto.Route
        |> RuleFinder.find
        |> RuleFinder.convertToResult
        |> Option.bind (fun r -> toDto dto r |> Some)
    

    let loadGenForm () =
        Substance.load ()
        GenPresProduct.load ()
        DoseRule.load ()
        ATCGroup.load ()