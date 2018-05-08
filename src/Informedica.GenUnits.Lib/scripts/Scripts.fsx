
#load "references.fsx"

#time 


module ValueUnit = 

    open MathNet.Numerics

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL


    type Value = BigRational
    type Name = string

    type ValueUnit = ValueUnit of  Value * Unit
    and Unit =
        | NoUnit
        | CombiUnit of Unit * Operator * Unit
        | General of GeneralUnit
        | Count of CountUnit
        | Mass of MassUnit
        | Volume of VolumeUnit
        | Time of TimeUnit
        | Molar of MolarUnit
        | InterNatUnit of IUnit
        | Weight of WeightUnit
        | Height of HeightUnit
        | BSA of BSAUnit
    and GeneralUnit = 
        | Quantity of (Name * Value)
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
    and Operator =
        | OpTimes
        | OpPer
        | OpPlus
        | OpMinus
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

    let opToStr op =
        match op with
        | OpPer -> "/"
        | OpTimes -> "x"
        | OpPlus -> "+"
        | OpMinus -> "-"


    module Group =

        type Group =
            | NoGroup
            | GeneralGroup
            | CountGroup 
            | MassGroup
            | VolumeGroup
            | TimeGroup
            | MolarGroup
            | InterNatUnitGroup
            | WeightGroup
            | HeightGroup
            | BSAGroup
            | CombiGroup of (Group * Operator * Group)


        let unitToGroup u =
            let rec get u = 
                match u with
                    | NoUnit         -> NoGroup
                    | General _      -> GeneralGroup
                    | Count _        -> CountGroup
                    | Mass _         -> MassGroup
                    | Volume _       -> VolumeGroup
                    | Time _         -> TimeGroup
                    | Molar _        -> MolarGroup
                    | InterNatUnit _ -> InterNatUnitGroup
                    | Weight _       -> WeightGroup
                    | Height _       -> HeightGroup
                    | BSA _          -> BSAGroup
                    | CombiUnit (ul, op, ur) ->
                        (get ul, op, get ur) |> CombiGroup
            
            get u


        let eqsGroup u1 u2 =
            if u1 = u2 then true
            else
                let g1 = u1 |> unitToGroup
                let g2 = u2 |> unitToGroup

                if g1 = GeneralGroup || g2 = GeneralGroup then false
                else g1 = g2
                    
        let toString g =
            let rec str g s =
                match g with 
                | NoGroup -> ""
                | GeneralGroup -> "General"
                | CountGroup -> "Count"
                | MassGroup -> "Mass"
                | VolumeGroup -> "Volume"
                | TimeGroup -> "Time"
                | MolarGroup -> "Molar"
                | InterNatUnitGroup -> "Internat. Unit"
                | WeightGroup -> "Weight"
                | HeightGroup -> "Height"
                | BSAGroup -> "BSA"
                | CombiGroup (gl, op, gr) ->
                    let gls = str gl s
                    let grs = str gr s

                    gls + (op |> opToStr) + grs
            
            str g ""

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

        let getMultiplier u = 
            let rec get u m =
                match u with
                | NoUnit -> one
                | General g -> 
                    match g with
                    | Quantity (_, n) -> n * one
                | Count g ->
                    match g with
                    | Times n -> n * one
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
                | CombiUnit (u1, op, u2) ->
                    let m1 = get u1 m
                    let m2 = get u2 m

                    match op with
                    | OpTimes -> m1 * m2 
                    | OpPer   -> m1 / m2 
                    | OpMinus | OpPlus -> m

            get u 1N

    let create v u : ValueUnit = (v, u) |> ValueUnit


    let get (ValueUnit (v, u)) = v, u


    let isCountUnit = Group.eqsGroup (1N |> Times |> Count)
    
                        
    let toBase (ValueUnit (v, u)) = v |> Multipliers.toBase (u |> Multipliers.getMultiplier)


    let toUnit (ValueUnit (v, u)) = v |> Multipliers.toUnit (u |> Multipliers.getMultiplier)


    let count = 1N |> Times |> Count


    let createCombiUnit u1 op u2 =
        match u1 |> isCountUnit, u2 |> isCountUnit with
        | true,  true  -> count
        | true,  false -> u2 
        | false, true  -> u1 
        | false, false -> (u1, op, u2) |> CombiUnit


    let remove rm u =
        let toCombi = createCombiUnit

        let rec rem u rm =
            let eqs = Group.eqsGroup rm

            match u with 
            | CombiUnit (u1, op, u2) ->
                match u1 |> eqs,  u2 |> eqs with
                | true,  true  -> count
                | false, true  -> u1
                | true,  false -> u2
                | false, false -> 
                    toCombi (rem u1 rm) op (rem u2 rm)
            | _ -> 
                if u |> eqs then count
                else u
    
        rem u rm


    let hasUnit u2 u1 =
        let rec find u =
            match u with
            | CombiUnit (lu, _, ru) ->
                if lu = u2 || ru = u2 then true
                else 
                    find lu || (find ru)
            | _ -> 
                u = u2
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
                | CombiUnit (ul, op, ur) ->
                    let op =
                        match op with
                        | OpPer -> op |> OpDivItem
                        | OpPlus | OpMinus -> op |> OpPlusMinItem
                        | OpTimes -> op |> OpMultItem
                    (toList ul) @ [ op ] @ (toList ur)
                | _ -> [ u |> UnitItem ]
    
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
                u1 |> Group.eqsGroup u2
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
                if u1 |> Group.eqsGroup u2 then u2
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

        type Localization = English | Dutch


        type Verbal = Long | Short


        type Language = 
            {
                Eng : string
                Dut : string
            }

                    
        let getDutch (lang : Language) = lang.Dut


        let getEnglish (lang : Language) = lang.Eng


        type UnitDetails =
            {
                Unit : Unit
                Group : Group.Group
                Abbreviation : Language
                Name : Language
                Synonyms : string list
            }

        
        let apply f (ud : UnitDetails) = f ud


        let get = apply id


        let getUnit ud = (ud |> get).Unit


        let create un gr ab nm sy = 
            {
                Unit = un
                Group = gr
                Abbreviation = ab
                Name = nm
                Synonyms = sy
            }


        let createGeneral n v = 
            let un = (n, v) |> Quantity |> General
            let ab = { Eng = n; Dut = n }
            let nm = { Eng = n; Dut = n }

            create un Group.GeneralGroup ab nm []


        let getGroup ud = (ud |> get).Group


        let getName ud = (ud |> get).Name


        let getAbbreviation ud = (ud |> get).Abbreviation

        
        let getEnglishName = getName >> getEnglish

        
        let getDutchName = getName >> getDutch

        
        let getEnglishAbbreviation = getAbbreviation >> getEnglish

        
        let getDutchAbbreviation = getAbbreviation >> getDutch


        module Count =

            let toCount = Count
            let times  = 1N |> Times |> toCount
        
        module Mass =

            let toMass = Mass

            let kiloGram = 1N |> KiloGram |> toMass
            let gram = 1N |> Gram |> toMass
            let milliGram = 1N |> MilliGram |> toMass
            let microGram = 1N |> MicroGram |> toMass
            let nanoGram = 1N |> NanoGram |> toMass
    
        module Weight =

            let toWeight = Weight

            let kiloGram = 1N |> WeightKiloGram |> toWeight
            let gram = 1N |> WeightGram |> toWeight

        module Volume =

            let toVolume = Volume

            let liter =  1N |>Liter |> toVolume
            let deciLiter =  1N |>DeciLiter |> toVolume
            let milliLiter =  1N |>MilliLiter |> toVolume
            let microLiter =  1N |>MicroLiter |> toVolume

        module Time =

            let toTime = Time

            let year = 1N |>Year |> toTime
            let month = 1N |>Month |> toTime
            let week = 1N |>Week |> toTime
            let day = 1N |>Day |> toTime
            let hour = 1N |>Hour |> toTime
            let minute = 1N |>Minute |> toTime
            let second = 1N |>Second |> toTime

        module Molar =

            let toMolar  = Molar

            let mol = 1N |>  Mol |> toMolar
            let milliMol = 1N |> MilliMol |> toMolar

        module InterNatUnit =
            
            let toInterNatUnit = InterNatUnit

            let MIU = 1N |> MIU |> toInterNatUnit
            let IU = 1N |> IU |> toInterNatUnit
        
        module Height =
            
            let toHeight = Height

            let meter = 1N |>  HeightMeter |> toHeight
            let centiMeter = 1N |> HeightCentiMeter |> toHeight

        module BSA =
            
            let toBSA = BSA

            let M2 = 1N |> M2 |> toBSA


        let units =
            [
                { Unit = Count.times; Group = Group.NoGroup;  Abbreviation = { Eng = "x"; Dut = "x" }; Name = { Eng = "times"; Dut = "keer" }; Synonyms = [] }

                { Unit = Mass.kiloGram; Group = Group.NoGroup;  Abbreviation = { Eng = "kg"; Dut = "kg" }; Name = { Eng = "kilogram"; Dut = "kilogram" }; Synonyms = [] }
                { Unit = Mass.gram; Group = Group.NoGroup;  Abbreviation = { Eng = "g"; Dut = "g" }; Name = { Eng = "gram"; Dut = "gram" }; Synonyms = ["gr"] }
                { Unit = Mass.milliGram; Group = Group.NoGroup;  Abbreviation = { Eng = "mg"; Dut = "mg" }; Name = { Eng = "milligram"; Dut = "milligram" }; Synonyms = ["millig"; "milligr"] }             
                { Unit = Mass.microGram; Group = Group.NoGroup;  Abbreviation = { Eng = "microg"; Dut = "microg" }; Name = { Eng = "microgram"; Dut = "microgram" }; Synonyms = ["mcg"; "µg"; "mcgr"] }             
                { Unit = Mass.microGram; Group = Group.NoGroup;  Abbreviation = { Eng = "nanog"; Dut = "nanog" }; Name = { Eng = "nanogram"; Dut = "nanogram" }; Synonyms = ["nanogr"; "ng"] }             

                { Unit = Volume.liter; Group = Group.NoGroup;  Abbreviation = { Eng = "l"; Dut = "l" }; Name = { Eng = "liter"; Dut = "liter" }; Synonyms = ["ltr"] }             
                { Unit = Volume.deciLiter; Group = Group.NoGroup;  Abbreviation = { Eng = "dl"; Dut = "dl" }; Name = { Eng = "deciliter"; Dut = "deciliter" }; Synonyms = ["decil"] }             
                { Unit = Volume.milliLiter; Group = Group.NoGroup;  Abbreviation = { Eng = "ml"; Dut = "ml" }; Name = { Eng = "milliliter"; Dut = "milliliter" }; Synonyms = ["millil"] }             
                { Unit = Volume.microLiter; Group = Group.NoGroup;  Abbreviation = { Eng = "microl"; Dut = "microl" }; Name = { Eng = "microliter"; Dut = "microliter" }; Synonyms = ["µl"] }             

                { Unit = Time.year; Group = Group.NoGroup;  Abbreviation = { Eng = "yr"; Dut = "jr" }; Name = { Eng = "year"; Dut = "jaar" }; Synonyms = [] }
                { Unit = Time.month; Group = Group.NoGroup;  Abbreviation = { Eng = "mo"; Dut = "mnd" }; Name = { Eng = "month"; Dut = "maand" }; Synonyms = [] }
                { Unit = Time.week; Group = Group.NoGroup;  Abbreviation = { Eng = "wk"; Dut = "wk" }; Name = { Eng = "week"; Dut = "week" }; Synonyms = [] }
                { Unit = Time.day; Group = Group.NoGroup;  Abbreviation = { Eng = "d"; Dut = "d" }; Name = { Eng = "day"; Dut = "dag" }; Synonyms = [] }
                { Unit = Time.hour; Group = Group.NoGroup;  Abbreviation = { Eng = "hr"; Dut = "u" }; Name = { Eng = "hour"; Dut = "uur" }; Synonyms = [] }
                { Unit = Time.minute; Group = Group.NoGroup;  Abbreviation = { Eng = "min"; Dut = "min" }; Name = { Eng = "minute"; Dut = "minuut" }; Synonyms = [] }
                { Unit = Time.second; Group = Group.NoGroup;  Abbreviation = { Eng = "s"; Dut = "s" }; Name = { Eng = "second"; Dut = "seconde" }; Synonyms = [] }
                             
                { Unit = Molar.mol; Group = Group.NoGroup;  Abbreviation = { Eng = "mol"; Dut = "mol" }; Name = { Eng = "mol"; Dut = "mol" }; Synonyms = [] }
                { Unit = Molar.milliMol; Group = Group.NoGroup;  Abbreviation = { Eng = "mmol"; Dut = "mmol" }; Name = { Eng = "millimol"; Dut = "millimol" }; Synonyms = [] }

                { Unit = Weight.kiloGram; Group = Group.NoGroup;  Abbreviation = { Eng = "kg"; Dut = "kg" }; Name = { Eng = "kilogram"; Dut = "kilogram" }; Synonyms = [] }
                { Unit = Weight.gram; Group = Group.NoGroup;  Abbreviation = { Eng = "g"; Dut = "g" }; Name = { Eng = "gram"; Dut = "gram" }; Synonyms = ["gr"] }

                { Unit = BSA.M2; Group = Group.NoGroup;  Abbreviation = { Eng = "m2"; Dut = "m2" }; Name = { Eng = "square meter"; Dut = "vierkante meter" }; Synonyms = ["gr"] }
            
            ]
            |> List.map (fun ud -> { ud with Group = ud.Unit |> Group.unitToGroup })

        let tryFind u =
            match units |> List.tryFind (fun udt -> udt.Unit = u) with
            | Some udt -> Some udt
            | None     -> None 


    let toString vu = 
        let v, u = vu |> get

        let rec str u s =
            match u with
            | NoUnit -> ""
            | CombiUnit (ul, op, ur) ->
                let uls = str ul s
                let urs = str ur s

                uls + (op |> opToStr) + urs
            | _ ->
                match Units.tryFind u with
                | Some udt -> udt.Name.Eng + "[" + (udt.Group |> Group.toString) + "]"
                | None     -> ""
                    
        
        (v |> BigRational.toString) + " " + (str u "")



module Tests =

    open MathNet.Numerics

    open ValueUnit

    let (>>!) u f =
        u |> toString |> printfn "%s"
        f u

    let mg400 = Units.Mass.milliGram |> create 400N
    let ml50  = Units.Volume.milliLiter  |> create 50N


    ((mg400 + mg400)/ ml50) 
    >>! ((*) ml50)
    >>! (fun vu -> vu / ml50)
    >>! ((*) ml50)
    >>! toString

    ((Units.Volume.liter |> create 1N) + (Units.Volume.milliLiter |> create 500N))


