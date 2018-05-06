
#load "references.fsx"

#time 


module ValueUnit = 

    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL


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



    module Group =

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

        let getUnitGroupMultiplier = function 
            | Count g ->
                match g with
                | Times n -> n * n * one
            | Mass g  ->
                match g with
                | KiloGram n  -> n * kilo
                | Gram n      -> n * one
                | MilliGram n -> n * milli
                | MicroGram n -> n * micro
                | NanoGram n  -> n * nano
            | Volume g  ->
                match g with
                | Liter n      -> n * one
                | DeciLiter n  -> n * deci
                | MilliLiter n -> n * milli
                | MicroLiter n -> n * micro
            | Time g  ->
                match g with
                | Year n   -> n * year
                | Month n  -> n * month
                | Week n   -> n * week
                | Day n    -> n * day
                | Hour n   -> n * hour
                | Minute n -> n * minute
                | Second n -> n * second
            | Molar g ->
                match g with
                | Mol n      -> n * one
                | MilliMol n -> n * milli
            | InterNatUnit g ->
                match g with
                | MIU n -> n * kilo * kilo
                | IU n  -> n * one
            | Weight g -> 
                match g with
                | WeightKiloGram n -> n * kilo
                | WeightGram n     -> n * one
            | Height g -> 
                match g with
                | HeightMeter n      -> n * one
                | HeightCentiMeter n -> n * centi
            | BSA g -> 
                match g with
                | M2 n -> n * one


    let create v u : ValueUnit = (v, u) |> ValueUnit


    let get (ValueUnit (v, u)) = v, u


    let eqsGroup u1 u2 =
        let rec eqs b u1 u2 =
            if not b then false
            else
                match u1, u2 with
                | NoUnit, NoUnit   -> false
                | Unit u1, Unit u2 -> 
                    let g1 = u1 |> Group.getUnitGroup
                    let g2 = u2 |> Group.getUnitGroup
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
            | Unit ug -> ug |> Multipliers.getUnitGroupMultiplier
            | CombiUnit (u1, op, u2) ->
                let m1 = get u1 m
                let m2 = get u2 m

                match op with
                | OpTimes -> m1 * m2 
                | OpPer   -> m1 / m2 
                | OpMinus | OpPlus -> m

        get u 1N

                        
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



    module private UnitItem =

        type UnitItem =
            | UnitItem of Unit
            | OpPlusMinItem of Operator
            | OpMultItem of Operator
            | OpDivItem of Operator


        let unitToList u =
            let rec toList u =
                match u with
                | NoUnit | Unit _ -> [ u |> UnitItem ]
                | CombiUnit (ul, op, ur) ->
                    let op =
                        match op with
                        | OpPer -> op |> OpDivItem
                        | OpPlus | OpMinus -> op |> OpPlusMinItem
                        | OpTimes -> op |> OpMultItem
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
                        | _-> NoUnit
                        |> toUnit rest
                    | _ -> 
                        match ul with
                        | oi::ui::rest ->
                            match oi, ui with
                            | OpDivItem op,     UnitItem ur
                            | OpPlusMinItem op, UnitItem ur
                            | OpMultItem op,    UnitItem ur ->
                                createCombiUnit u op ur
                                |> toUnit rest
                            | _ -> u
                        | _ -> u

            toUnit ul NoUnit


        let eqs ui1 ui2 =
            match ui1, ui2 with
            | UnitItem u1, UnitItem u2 ->
                u1 |> eqsGroup u2
            | _ -> false


        let isUnitItem ui = 
            match ui with
            | UnitItem _ -> true
            | _          -> false


    let simplify vu =
        let (_, u) = vu |> get
        let v = vu |> toBase

        let opDiv = OpPer |> UnitItem.OpDivItem

        let eqs = UnitItem.eqs

        let isUnitItem = UnitItem.isUnitItem

        let rec simpl acc ul =       
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
                                let ulr = ulr |> List.remove (eqs ui)
                                (ull, ulr)
                            else (ui::ull, ulr)
                        ) ([], ulr)
            
                    simpl (acc @ ull) ulr

        u
        |> UnitItem.unitToList
        |> simpl []
        |> UnitItem.listToUnit
        |> create v
        |> (fun vu -> 
            let _, u = vu |> get
            let v = vu |> toUnit
            create v u
        )


    let calc op vu1 vu2 = 

        let (ValueUnit (_, u1)) = vu1
        let (ValueUnit (_, u2)) = vu2

        let v = vu1 |> toBase |> op <| (vu2 |> toBase)
    
        let u =
            match op with
            | BigRational.Mult    -> (u1, OpTimes, u2) |> CombiUnit
            | BigRational.Div     -> (u1, OpPer,   u2) |> CombiUnit
            | BigRational.Add
            | BigRational.Subtr   -> 
                if u1 |> eqsGroup u2 then u2
                else
                    failwith "cannot add or subtract different units"
            | BigRational.NoMatch -> failwith "invalid operator"

        create v u
        |> toUnit
        |> (fun v -> create v u)    
        |> simplify


    type ValueUnit with
         
        static member (*) (vu1, vu2) = calc (*) vu1 vu2

        static member (/) (vu1, vu2) = calc (/) vu1 vu2

        static member (+) (vu1, vu2) = calc (+) vu1 vu2

        static member (-) (vu1, vu2) = calc (-) vu1 vu2


    module Units =

        module Count =

            let toCount = Count >> Unit
            let times  = 1N |> Times |> toCount
        
        module Mass =

            let toMass = Mass >> Unit

            let kiloGram = 1N |> KiloGram |> toMass
            let gram = 1N |> Gram |> toMass
            let milliGram = 1N |> MilliGram |> toMass
            let microGram = 1N |> MicroGram |> toMass
            let nanoGram = 1N |> NanoGram |> toMass
    
        module Weight =

            let toWeight = Weight >> Unit

            let kiloGram = 1N |> WeightKiloGram |> toWeight
            let gram = 1N |> WeightGram |> toWeight

        module Volume =

            let toVolume = Volume >> Unit

            let liter =  1N |>Liter |> toVolume
            let deciLiter =  1N |>DeciLiter |> toVolume
            let milliLiter =  1N |>MilliLiter |> toVolume
            let microLiter =  1N |>MicroLiter |> toVolume

        module Time =

            let toTime = Time >> Unit

            let year = 1N |>Year |> toTime
            let month = 1N |>Month |> toTime
            let week = 1N |>Week |> toTime
            let day = 1N |>Day |> toTime
            let hour = 1N |>Hour |> toTime
            let minute = 1N |>Minute |> toTime
            let second = 1N |>Second |> toTime

        module Molar =

            let toMolar  = Molar >> Unit

            let mol = 1N |>  Mol |> toMolar
            let milliMol = 1N |> MilliMol |> toMolar

        module InterNatUnit =
            
            let toInterNatUnit = InterNatUnit >> Unit

            let MIU = 1N |> MIU |> toInterNatUnit
            let IU = 1N |> IU |> toInterNatUnit
        
        module Height =
            
            let toHeight = Height >> Unit

            let meter = 1N |>  HeightMeter |> toHeight
            let centiMeter = 1N |> HeightCentiMeter |> toHeight

        module BSA =
            
            let toBSA = BSA >> Unit

            let M2 = 1N |> M2 |> toBSA




module Tests =

    open MathNet.Numerics

    open ValueUnit

    let mg400 = Units.Mass.milliGram |> create 400N
    let ml50  = Units.Volume.milliLiter  |> create 50N


    ((mg400 + mg400)/ ml50) 
    |> ((*) ml50)
    |> (fun vu -> vu / ml50)
    |> ((*) ml50)




