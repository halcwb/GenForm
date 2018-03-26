namespace Informedica.GenUnits.Lib

module Api =

    open MathNet.Numerics
    open Informedica.GenUtils.Lib.BCL

    module US = Unit.Units
    module C = Constants
    module CU = CombiUnit
    module VU = ValueUnit

    type Unit = 
        | Count_Times
        | Mass_KiloGram
        | Mass_Gram
        | Mass_MilliGram
        | Mass_MicroGram
        | Mass_NanoGram
        | Molar_Mol
        | Molar_MilliMol
        | Weight_KiloGram
        | Weight_Gram
        | Bsa_M2
        | Volume_Liter
        | Volume_DeciLiter
        | Volume_MilliLiter
        | Volume_MicroLiter
        | Time_Second
        | Time_Minute
        | Time_Hour
        | Time_Day
        | Time_Week
        | Time_Month
        | Time_Year
        | Distance_Meter
        | Distance_Centimeter

    let create f v u = 
        match u with
        | Count_Times -> US.Times |> US.toCount 
        | Mass_KiloGram  -> US.KiloGram  |> US.toMass 
        | Mass_Gram      -> US.Gram      |> US.toMass 
        | Mass_MilliGram -> US.MilliGram |> US.toMass 
        | Mass_MicroGram -> US.MicroGram |> US.toMass 
        | Mass_NanoGram  -> US.NanoGram  |> US.toMass
        | Molar_Mol      -> US.Mol      |> US.toMolar
        | Molar_MilliMol -> US.MilliMol |> US.toMolar
        | Weight_KiloGram -> US.WeightKg |> US.toWeight
        | Weight_Gram -> US.WeightGr |> US.toWeight
        | Bsa_M2 -> US.Bsa |> US.toBsa
        | Volume_Liter -> US.Liter           |> US.toVolume
        | Volume_DeciLiter -> US.DeciLiter   |> US.toVolume
        | Volume_MilliLiter -> US.MilliLiter |> US.toVolume
        | Volume_MicroLiter -> US.MicroLiter |> US.toVolume
        | Time_Second -> US.Second |> US.toTime
        | Time_Minute  -> US.Minute |> US.toTime
        | Time_Hour   -> US.Hour   |> US.toTime
        | Time_Day    -> US.Day    |> US.toTime
        | Time_Week   -> US.Week   |> US.toTime
        | Time_Month  -> US.Month  |> US.toTime
        | Time_Year   -> US.Year   |> US.toTime
        | Distance_Meter      -> US.Meter      |> US.toDistance
        | Distance_Centimeter -> US.Centimeter |> US.toDistance
        |> f v

    let createVU v u = create (fun v u -> u |> CU.create 1N |> VU.create v) v u
    
    let createCU v u = create (fun v u -> u |> CU.create v) v u

    let fromString = VU.fromString

    let toString = VU.toString

    let eval s = 
        let addSpace s = C.space + s + C.space
        let mults  = C.mults |> addSpace
        let divs   = C.divs  |> addSpace
        let adds   = "+"     |> addSpace
        let subtrs = "-"     |> addSpace 

        let del = "#"
        let addDel s = del + s + del

        let opts s = 
            match s with
            | _ when s = C.mults -> (*)
            | _ when s = C.divs  -> (/)
            | _ when s = "+"     -> (+)
            | _ when s = "-"     -> (-)
            | _ -> failwith "Cannot evaluate string"

        let rec eval' acc terms =
            if acc |> Option.isNone then 
                eval' (terms |> List.head |> VU.fromString |> Some) (terms |> List.tail)
            else
                match terms with
                | [] -> acc |> Option.get
                | os::vus::rest ->
                    let op = os |> opts
                    let vu = vus |> VU.fromString
                    rest |> eval' ((acc |> Option.get) |> op <| vu |> Some) 
                | _ -> failwith "Cannot evaluate string"          

        s 
        |> String.replace mults  (C.mults |> addDel)
        |> String.replace divs   (C.divs  |> addDel)
        |> String.replace adds   (adds    |> addDel)
        |> String.replace subtrs (subtrs  |> addDel)
        |> String.split del
        |> eval' None
        |> VU.toString

    let convert s2 s1 = 
        let vu = s1 |> VU.fromString
        let cu = s2 |> CU.fromString
        vu 
        |> VU.convertTo cu
        |> toString

    let (>>!) vu cu = VU.convertTo cu vu
    
    type BigRational with
        
        member x.toVU u = createVU x u

        member x.toCU u = createCU x u