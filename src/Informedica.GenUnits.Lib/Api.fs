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
        | Height_Meter
        | Height_Centimeter

    let create f v u = 
        match u with
        | Count_Times -> Unit.Times |> Unit.Count 
        | Mass_KiloGram  -> Unit.KiloGram  |> Unit.Mass 
        | Mass_Gram      -> Unit.Gram      |> Unit.Mass 
        | Mass_MilliGram -> Unit.MilliGram |> Unit.Mass 
        | Mass_MicroGram -> Unit.MicroGram |> Unit.Mass 
        | Mass_NanoGram  -> Unit.NanoGram  |> Unit.Mass
        | Molar_Mol      -> Unit.Mol      |> Unit.Molar
        | Molar_MilliMol -> Unit.MilliMol |> Unit.Molar
        | Weight_KiloGram -> Unit.WeightKilogram |> Unit.Weight
        | Weight_Gram -> Unit.WeightGram |> Unit.Weight
        | Bsa_M2 -> Unit.M2 |> Unit.BSA
        | Volume_Liter -> Unit.Liter           |> Unit.Volume
        | Volume_DeciLiter -> Unit.DeciLiter   |> Unit.Volume
        | Volume_MilliLiter -> Unit.MilliLiter |> Unit.Volume
        | Volume_MicroLiter -> Unit.MicroLiter |> Unit.Volume
        | Time_Second -> Unit.Second |> Unit.Time
        | Time_Minute -> Unit.Minute |> Unit.Time
        | Time_Hour   -> Unit.Hour   |> Unit.Time
        | Time_Day    -> Unit.Day    |> Unit.Time
        | Time_Week   -> Unit.Week   |> Unit.Time
        | Time_Month  -> Unit.Month  |> Unit.Time
        | Time_Year   -> Unit.Year   |> Unit.Time
        | Height_Meter      -> Unit.Meter      |> Unit.Height
        | Height_Centimeter -> Unit.CentiMeter |> Unit.Height
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


