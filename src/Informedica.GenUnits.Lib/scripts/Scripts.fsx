
#load "references.fsx"

#time 

open MathNet.Numerics
open Giraffe

open Informedica.GenUtils.Lib



module Multipliers =

    let one = 1N
    let kilo = 1000N
    let deci = 1N / 10N
    let centi = deci / 10N
    let milli = 1N / kilo
    let micro = milli / kilo                                                                                            
    let nano = micro / kilo

    let second = 1N
    let minute = 60N * second
    let hour = minute * minute
    let day = 24N * hour
    let week = 7N * day
    let month = 4N * week
    let year = 365N * day 

    let inline toBase m v  = v * m
    let inline toUnit m v  = v / m


type Value = BigRational

type ValueUnit = ValueUnit of  Value * Unit
and Unit = 
    | NoUnit
    | Unit of UnitGroup
    | CombiUnit of Unit * Operator * Unit
and Operator =
    | OpTimes
    | OpPer
    | OpPlus
    | OpMinus
and UnitGroup =
    | Count of CountUnit
    | Mass of MassUnit
    | Volume of VolumeUnit
    | Time of TimeUnit
    | Molar of MolarUnit
    | InterNatUnit of IUnit
    | Weight of WeightUnit
    | Height of HeightUnit
    | BSA of BSAUnit
and CountUnit = 
    | Times of Times
and MassUnit = 
    | KiloGram of KiloGram
    | Gram of Gram
    | MilliGram of MilliGram
    | MicroGram of MicroGram
    | NanoGram of NanoGram
and VolumeUnit =
    | Liter of Liter
    | DeciLiter of DeciLiter
    | MilliLiter of MilliLiter
    | MicroLiter of MicroLiter
and TimeUnit =
    | Year of Year
    | Month of Month
    | Week of Week
    | Day of Day
    | Hour of Hour
    | Minute of Minute
    | Second of Second
and MolarUnit =
    | Mol of Mol
    | MilliMol of MilliMol
and IUnit =
    | MIU of MIU
    | IU of IU
and WeightUnit = 
    | WeightKiloGram of KiloGram
    | WeightGram of Gram
and HeightUnit = 
    | HeightMeter of Meter
    | HeightCentiMeter of CentiMeter
and BSAUnit = 
    | M2 of M2
// Count
and Times = BigRational
// InterNatUnit
and IU  = BigRational
and MIU = BigRational
// Mass
and KiloGram  = BigRational
and Gram      = BigRational
and MilliGram = BigRational
and MicroGram = BigRational
and NanoGram  = BigRational
// Volume
and Liter      = BigRational
and DeciLiter  = BigRational
and MilliLiter = BigRational
and MicroLiter = BigRational
// Time
and Second = BigRational
and Minute = BigRational
and Hour   = BigRational
and Day    = BigRational
and Week   = BigRational
and Month  = BigRational
and Year   = BigRational
// Height
and CentiMeter = BigRational
and Meter      = BigRational
// Molar
and Mol      = BigRational
and MilliMol = BigRational
// BSA
and M2 = BigRational

type Group =
    | CountGroup 
    | MassGroup
    | VolumeGroup
    | TimeGroup
    | MolarGroup
    | InterNatUnitGroup
    | WeightGroup
    | HeightGroup
    | BSAGroup

let create v u : ValueUnit = (v, u) |> ValueUnit

let get (ValueUnit (v, u)) = v, u

1N |> WeightKiloGram |> Weight |> Unit |> create 10N


let getUnitGroupMultiplier = function 
    | Count g ->
        match g with
        | Times n -> n * n * Multipliers.one
    | Mass g  ->
        match g with
        | KiloGram n  -> n * Multipliers.kilo
        | Gram n      -> n * Multipliers.one
        | MilliGram n -> n * Multipliers.milli
        | MicroGram n -> n * Multipliers.micro
        | NanoGram n  -> n * Multipliers.nano
    | Volume g  ->
        match g with
        | Liter n      -> n * Multipliers.one
        | DeciLiter n  -> n * Multipliers.deci
        | MilliLiter n -> n * Multipliers.milli
        | MicroLiter n -> n * Multipliers.micro
    | Time g  ->
        match g with
        | Year n   -> n * Multipliers.year
        | Month n  -> n * Multipliers.month
        | Week n   -> n * Multipliers.week
        | Day n    -> n * Multipliers.day
        | Hour n   -> n * Multipliers.hour
        | Minute n -> n * Multipliers.minute
        | Second n -> n * Multipliers.second
    | Molar g ->
        match g with
        | Mol n      -> n * Multipliers.one
        | MilliMol n -> n * Multipliers.milli
    | InterNatUnit g ->
        match g with
        | MIU n -> n * Multipliers.kilo * Multipliers.kilo
        | IU n  -> n * Multipliers.one
    | Weight g -> 
        match g with
        | WeightKiloGram n -> n * Multipliers.kilo
        | WeightGram n     -> n * Multipliers.one
    | Height g -> 
        match g with
        | HeightMeter n      -> n * Multipliers.one
        | HeightCentiMeter n -> n * Multipliers.centi
    | BSA g -> 
        match g with
        | M2 n -> n * Multipliers.one


let getUnitGroup = function
    | Count _        -> CountGroup
    | Mass _         -> MassGroup
    | Volume _       -> VolumeGroup
    | Time _         -> TimeGroup
    | Molar _        -> MolarGroup
    | InterNatUnit _ -> InterNatUnitGroup
    | Weight _       -> WeightGroup
    | Height _       -> HeightGroup
    | BSA _          -> BSAGroup


let eqsGroup u1 u2 =
    let rec eqs b u1 u2 =
        if not b then false
        else
            match u1, u2 with
            | NoUnit, NoUnit   -> false
            | Unit u1, Unit u2 -> 
                let g1 = u1 |> getUnitGroup
                let g2 = u2 |> getUnitGroup
                g1 = g2 && b
            | CombiUnit (u11, op1, u12), CombiUnit (u21, op2, u22) ->
                if op1 = op2 |> not then false
                else
                    (eqs b u11 u21) && (eqs b u12 u22)
            | _ -> false
    
    eqs true u1 u2


let isCountUnit = eqsGroup (1N |> Times |> Count  |> Unit)
    

let getMultiplier u =
    let rec get u m = 
        match u with
        | NoUnit  -> m 
        | Unit ug -> ug |> getUnitGroupMultiplier
        | CombiUnit (u1, op, u2) ->
            let m1 = get u1 m
            let m2 = get u2 m

            match op with
            | OpTimes -> m1 * m2 
            | OpPer   -> m1 / m2 
            | OpMinus | OpPlus -> m

    get u 1N



let (|Mult|Div|Add|Subtr|) op =
    match op with
    | _ when 1N |> op <| 2N = 2N      -> Mult
    | _ when 1N |> op <| 2N = (1N/2N) -> Div
    | _ when 1N |> op <| 2N = 3N      -> Add
    | _ when 1N |> op <| 2N = -1N     -> Subtr
    | _ -> failwith "Not a valid operator"

                        
let toBase (ValueUnit (v, u)) = v |> Multipliers.toBase (u |> getMultiplier)


let toUnit (ValueUnit (v, u)) = v |> Multipliers.toUnit (u |> getMultiplier)


let count = 1N |> Times |> Count |> Unit


let createCombiUnit u1 op u2 =
    match u1 |> isCountUnit, u2 |> isCountUnit with
    | true,  true  -> count
    | true,  false -> u2 
    | false, true  -> u1 
    | false, false -> (u1, op, u2) |> CombiUnit


let remove rm u =
    let toCombi = createCombiUnit

    let rec rem u rm =
        let eqs = eqsGroup rm

        match u with 
        | NoUnit 
        | Unit _ -> 
            if u |> eqs then count
            else u
        | CombiUnit (u1, op, u2) ->
            match u1 |> eqs,  u2 |> eqs with
            | true,  true  -> count
            | false, true  -> u1
            | true,  false -> u2
            | false, false -> 
                toCombi (rem u1 rm) op (rem u2 rm)
    
    rem u rm


let hasUnit u2 u1 =
    let rec find u =
        match u with
        | NoUnit | Unit _ -> 
            u = u2
        | CombiUnit (lu, _, ru) ->
            if lu = u2 || ru = u2 then true
            else 
                find lu || (find ru)
    find u1





let calc op vu1 vu2 = 

    let (ValueUnit (_, u1)) = vu1
    let (ValueUnit (_, u2)) = vu2

    let v = vu1 |> toBase |> op <| (vu2 |> toBase)
    
    let u =
        match op with
        | Mult  -> (u1, OpTimes, u2)
        | Div   -> (u1, OpPer,   u2)
        | Add   -> (u1, OpPlus,  u2)
        | Subtr -> (u1, OpMinus, u2)
        |> CombiUnit

    create v u
    |> toUnit
    |> (fun v -> create v u)


type ValueUnit with
         
    static member (*) (vu1, vu2) = calc (*) vu1 vu2

    static member (/) (vu1, vu2) = calc (/) vu1 vu2

    static member (+) (vu1, vu2) = calc (+) vu1 vu2

    static member (-) (vu1, vu2) = calc (-) vu1 vu2


let massG = Gram >> Mass >> Unit 
let massMG = MilliGram >> Mass >> Unit 
let massKG = KiloGram >> Mass >> Unit
let wghtKG = WeightKiloGram >> Weight >> Unit
let volL = Liter >> Volume >> Unit
let volML = MilliLiter >> Volume >> Unit

1N 
|> massKG
|> create 10N
|> toBase 

1N
|> massKG
|> create 10N
|> toUnit


2N 
|> massKG
|> eqsGroup (1N |> massKG)


let mg400 = 1N |> massMG |> create 400N
let ml50 = 1N |> volML |> create 50N


type UnitItem =
    | UnitItem of Unit
    | OperatorItem of Operator
    | OpDivItem of Operator


let unitToList u =
    let rec toList u =
        match u with
        | NoUnit | Unit _ -> [ u |> UnitItem ]
        | CombiUnit (ul, op, ur) ->
            let op =
                match op with
                | OpPer -> op |> OpDivItem
                | _     -> op |> OperatorItem
            (toList ul) @ [ op ] @ (toList ur)
    
    toList u


let listToUnit ul =
    let rec toUnit ul u =
        match ul with
        | []       -> u
        | ui::rest -> 
            match u with
            | NoUnit -> 
                match ui with
                | UnitItem u'    -> u'
                | OperatorItem _ | OpDivItem _-> NoUnit
                |> toUnit rest
            | _ -> 
                match ul with
                | oi::ui::rest ->
                    match oi, ui with
                    | OpDivItem op, UnitItem ur
                    | OperatorItem op, UnitItem ur ->
                        createCombiUnit u op ur
                        |> toUnit rest
                    | _ -> u
                | _ -> u

    toUnit ul NoUnit


let evaluate vu =
    let (ValueUnit (_, u)) = vu
    let v = vu |> toBase

    let opDiv = OpPer |> OpDivItem

    let eqs ui1 ui2 =
        match ui1, ui2 with
        | UnitItem u1, UnitItem u2 ->
            u1 |> eqsGroup u2
        | _ -> false

    let isUnitItem ui = 
        match ui with
        | UnitItem _ -> true
        | OperatorItem _ | OpDivItem _ -> false

    let rec simplify acc ul =
        printfn "simplifying to: %A" acc
        
        match ul with
        | [] -> acc
        | _ ->
            let ull, ulr = 
                match ul |> List.tryFindIndex ((=) opDiv) with
                | Some i -> ul |> List.splitAt i
                | None   -> [], ul
            if ull = List.empty then acc @ ulr
            else
                let ull, ulr =
                    ull
                    |> List.fold (fun acc ui ->
                        let ull, ulr = acc
           
                        if ui |> isUnitItem && ulr |> List.exists (eqs ui) then
                            printfn "removing: %A" ui
                            let ulr = ulr |> List.remove (eqs ui)
                            (ull, ulr)
                        else (ui::ull, ulr)
                    ) ([], ulr)
            
                simplify (acc @ ull) ulr

    u
    |> unitToList
    |> simplify []
    |> listToUnit
    |> create v
    |> (fun vu -> 
        let _, u = vu |> get
        let v = vu |> toUnit
        create v u
    )

(mg400 / ml50) 
|> toUnit

(mg400 / ml50) 
|> evaluate
|> ((*) ml50)
|> evaluate
|> (fun vu -> vu / ml50)
|> evaluate
|> ((*) ml50)
|> evaluate    




