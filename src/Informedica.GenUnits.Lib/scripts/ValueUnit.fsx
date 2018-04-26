#load @"references.fsx"


module Constants = 
        
        [<Literal>] 
        let generalGroup = "General"
        [<Literal>] 
        let countGroup = "Count"
        [<Literal>]
        let massGroup = "Mass"
        [<Literal>]
        let molarGroup = "Molar"
        [<Literal>]
        let weightGroup = "Weight"
        [<Literal>]
        let bsaGroup = "BSA"
        [<Literal>]
        let volumeGroup = "Volume"
        [<Literal>]
        let timeGroup = "Time"
        [<Literal>]
        let distanceGroup = "Distance"


        [<Literal>] 
        let mults = "*"
        [<Literal>] 
        let divs  = "/"
        [<Literal>]
        let empts = ""
        [<Literal>]
        let space = " "
        [<Literal>]
        let openBr = "["
        [<Literal>]
        let closBr = "]"



module Unit =

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL

    module Multipliers =

        open MathNet.Numerics

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

        let toBase m v  = v * m
        let toUnit m v  = v / m


    type Unit =
        | General of string
        | Count of CountUnit
        | Mass of MassUnit
        | Volume of VolumeUnit
        | Time of TimeUnit
        | Molar of MolarUnit
        | InternationalUnit of InternationalUnit
        | Weight of WeightUnit
        | Height of HeightUnit
        | BSA of BSAUnit
    and CountUnit = Times
    and MassUnit = 
        | KiloGram 
        | Gram 
        | MilliGram 
        | MicroGram 
        | NanoGram 
    and VolumeUnit =
        | Liter 
        | DeciLiter 
        | MilliLiter 
        | MicroLiter 
    and TimeUnit =
        | Year 
        | Month 
        | Week 
        | Day 
        | Hour 
        | Minute 
        | Second 
    and MolarUnit =
        | Mol
        | MilliMol 
    and InternationalUnit =
        | MiljIU
        | IU
    and WeightUnit = 
        | WeightKilogram
        | WeightGram
    and HeightUnit = 
        | Meter
        | CentiMeter
    and BSAUnit = 
        | M2


    let getMultiplier = function 
        | General _ -> Multipliers.one
        | Mass mg ->
            match mg with
            | KiloGram  -> Multipliers.kilo
            | Gram      -> Multipliers.one
            | MilliGram -> Multipliers.milli
            | MicroGram -> Multipliers.micro
            | NanoGram  -> Multipliers.nano
        | Volume vg ->
            match vg with
            | Liter      -> Multipliers.one
            | DeciLiter  -> Multipliers.deci
            | MilliLiter -> Multipliers.milli
            | MicroLiter -> Multipliers.micro
        | _ -> Multipliers.one


    let applyGroup falt fcnt fmss fvol ftme fmol fiun fwgt fhgt fbsa u =
        match u with
        | General alt       -> alt |> falt
        | Count cnt             -> cnt |> fcnt
        | Mass mss              -> mss |> fmss
        | Volume vol            -> vol |> fvol
        | Time tme              -> tme |> ftme
        | Molar mol             -> mol |> fmol
        | InternationalUnit iun -> iun |> fiun
        | Weight wgt            -> wgt |> fwgt
        | Height hgt            -> hgt |> fhgt
        | BSA bsa               -> bsa |> fbsa


    let eqGroup u1 u2 =
        let true_  = fun _ -> true
        let false_ = fun _ -> false

        let alt = match u1 with | General s -> s | _ -> ""

        let altEq = function
        | General s -> alt = s
        | _ -> false

        if u1 = u2 then true
        else 
            match u1 with
            | General _       -> u2 |> altEq
            | Count _             -> u2 |> applyGroup false_ true_  false_ false_ false_ false_ false_ false_ false_ false_
            | Mass _              -> u2 |> applyGroup false_ false_ true_  false_ false_ false_ false_ false_ false_ false_
            | Volume _            -> u2 |> applyGroup false_ false_ false_ true_  false_ false_ false_ false_ false_ false_
            | Time _              -> u2 |> applyGroup false_ false_ false_ false_ true_  false_ false_ false_ false_ false_
            | Molar _             -> u2 |> applyGroup false_ false_ false_ false_ false_ true_  false_ false_ false_ false_
            | InternationalUnit _ -> u2 |> applyGroup false_ false_ false_ false_ false_ false_ true_  false_ false_ false_
            | Weight _            -> u2 |> applyGroup false_ false_ false_ false_ false_ false_ false_ true_  false_ false_
            | Height _            -> u2 |> applyGroup false_ false_ false_ false_ false_ false_ false_ false_ true_  false_
            | BSA _               -> u2 |> applyGroup false_ false_ false_ false_ false_ false_ false_ false_ false_ true_


    let isCount u = match u with | Count _ -> true | _ -> false


    let count = Times |> Count


    let getGroupName = Reflection.toString


    module Units =


        type Localization = English | Dutch


        type Verbal = Long | Short


        type Language = 
            {
                Eng : string
                Dut : string
            }

                    
        let getDutch (l : Language) = l.Dut


        let getEnglish (l : Language) = l.Dut


        type UnitDetails =
            {
                Unit : Unit
                Group : string
                Abbreviation : Language
                Name : Language
                Synonyms : string list
            }

        
        let apply f (ud : UnitDetails) = f ud


        let get = apply id


        let getGroup ud = (ud |> get).Group


        let getName ud = (ud |> get).Name


        let getAbbreviation ud = (ud |> get).Abbreviation

        
        let getEnglishName = getName >> getEnglish

        
        let getDutchName = getName >> getDutch

        
        let getEnglishAbbreviation = getAbbreviation >> getEnglish

        
        let getDutchAbbreviation = getAbbreviation >> getDutch


        let countTimes = Count Times


        let massKiloGram = Mass KiloGram


        let massGram = Mass Gram


        let massMilliGram = Mass MilliGram


        let massMicroGram = Mass MicroGram


        let massNanoGram = Mass NanoGram


        let volumeLiter = Volume Liter


        let volumeDeciLiter = Volume DeciLiter

         
        let volumeMilliLiter = Volume MilliLiter

         
        let volumeMicroLiter = Volume MicroLiter 


        let timeYear = Time Year 


        let timeMonth = Time Month


        let timeWeek = Time Week


        let timeDay = Time Day


        let timeHour = Time Hour 


        let timeMinute = Time Minute

         
        let timeSecond = Time Second


        let molarMol = Molar Mol


        let molarMilliMol = Molar MilliMol 

                         
        let weightKg = Weight WeightKilogram


        let weightGram = Weight WeightGram


        let bsaM2 = BSA M2


        let units =
            [
                { Unit = countTimes; Group = "";  Abbreviation = { Eng = "x"; Dut = "x" }; Name = { Eng = "times"; Dut = "keer" }; Synonyms = [] }

                { Unit = massKiloGram; Group = "";  Abbreviation = { Eng = "kg"; Dut = "kg" }; Name = { Eng = "kilogram"; Dut = "kilogram" }; Synonyms = [] }
                { Unit = massGram; Group = "";  Abbreviation = { Eng = "g"; Dut = "g" }; Name = { Eng = "gram"; Dut = "gram" }; Synonyms = ["gr"] }
                { Unit = massMilliGram; Group = "";  Abbreviation = { Eng = "mg"; Dut = "mg" }; Name = { Eng = "milligram"; Dut = "milligram" }; Synonyms = ["millig"; "milligr"] }             
                { Unit = massMicroGram; Group = "";  Abbreviation = { Eng = "microg"; Dut = "microg" }; Name = { Eng = "microgram"; Dut = "microgram" }; Synonyms = ["mcg"; "µg"; "mcgr"] }             
                { Unit = massMicroGram; Group = "";  Abbreviation = { Eng = "nanog"; Dut = "nanog" }; Name = { Eng = "nanogram"; Dut = "nanogram" }; Synonyms = ["nanogr"; "ng"] }             

                { Unit = volumeLiter; Group = "";  Abbreviation = { Eng = "l"; Dut = "l" }; Name = { Eng = "liter"; Dut = "liter" }; Synonyms = ["ltr"] }             
                { Unit = volumeDeciLiter; Group = "";  Abbreviation = { Eng = "dl"; Dut = "dl" }; Name = { Eng = "deciliter"; Dut = "deciliter" }; Synonyms = ["decil"] }             
                { Unit = volumeMilliLiter; Group = "";  Abbreviation = { Eng = "ml"; Dut = "ml" }; Name = { Eng = "milliliter"; Dut = "milliliter" }; Synonyms = ["millil"] }             
                { Unit = volumeMicroLiter; Group = "";  Abbreviation = { Eng = "microl"; Dut = "microl" }; Name = { Eng = "microliter"; Dut = "microliter" }; Synonyms = ["µl"] }             

                { Unit = timeYear; Group = "";  Abbreviation = { Eng = "yr"; Dut = "jr" }; Name = { Eng = "year"; Dut = "jaar" }; Synonyms = [] }
                { Unit = timeMonth; Group = "";  Abbreviation = { Eng = "mo"; Dut = "mnd" }; Name = { Eng = "month"; Dut = "maand" }; Synonyms = [] }
                { Unit = timeWeek; Group = "";  Abbreviation = { Eng = "wk"; Dut = "wk" }; Name = { Eng = "week"; Dut = "week" }; Synonyms = [] }
                { Unit = timeDay; Group = "";  Abbreviation = { Eng = "d"; Dut = "d" }; Name = { Eng = "day"; Dut = "dag" }; Synonyms = [] }
                { Unit = timeHour; Group = "";  Abbreviation = { Eng = "hr"; Dut = "u" }; Name = { Eng = "hour"; Dut = "uur" }; Synonyms = [] }
                { Unit = timeMinute; Group = "";  Abbreviation = { Eng = "min"; Dut = "min" }; Name = { Eng = "minute"; Dut = "minuut" }; Synonyms = [] }
                { Unit = timeSecond; Group = "";  Abbreviation = { Eng = "s"; Dut = "s" }; Name = { Eng = "second"; Dut = "seconde" }; Synonyms = [] }
                             
                { Unit = molarMol; Group = "";  Abbreviation = { Eng = "mol"; Dut = "mol" }; Name = { Eng = "mol"; Dut = "mol" }; Synonyms = [] }
                { Unit = molarMilliMol; Group = "";  Abbreviation = { Eng = "mmol"; Dut = "mmol" }; Name = { Eng = "millimol"; Dut = "millimol" }; Synonyms = [] }

                { Unit = weightKg; Group = "";  Abbreviation = { Eng = "kg"; Dut = "kg" }; Name = { Eng = "kilogram"; Dut = "kilogram" }; Synonyms = [] }
                { Unit = weightGram; Group = "";  Abbreviation = { Eng = "g"; Dut = "g" }; Name = { Eng = "gram"; Dut = "gram" }; Synonyms = ["gr"] }

                { Unit = bsaM2; Group = "";  Abbreviation = { Eng = "m2"; Dut = "m2" }; Name = { Eng = "square meter"; Dut = "vierkante meter" }; Synonyms = ["gr"] }
            
            ]
            |> List.map (fun ud -> { ud with Group = ud.Unit |> getGroupName })


        let tryFind u g =
            let eqs = String.equalsCapInsens

            units
            |> List.tryFind (fun ud ->
                ud.Group |> eqs g &&
                (ud.Abbreviation.Eng |> eqs u ||
                 ud.Abbreviation.Dut |> eqs u ||
                 ud.Name.Eng |> eqs u ||
                 ud.Name.Dut |> eqs u ||
                 ud.Synonyms |> List.exists (eqs u))
            )

        let toString loc verb u =
            match units
                  |> List.tryFind (fun ud -> ud.Unit = u) with
            | Some ud -> 
                let g = Constants.openBr + (ud |> getGroup) + Constants.closBr
                match loc with
                | English -> 
                    match verb with
                    | Long -> ud |> getEnglishName
                    | Short -> ud |> getEnglishAbbreviation
                | Dutch -> 
                    match verb with
                    | Long -> ud |> getDutchName
                    | Short -> ud |> getDutchAbbreviation
                |> (fun s -> s + g)
            | None -> ""


    let getAbbreviation u =
        match Units.units
              |> List.tryFind (fun ud ->
                  ud.Unit = u
              ) with
        | Some ud -> ud.Abbreviation.Eng
        | None -> ""


    let fromString u g =
        Units.tryFind u g 
        |> Option.bind (fun ud -> ud.Unit |> Some)


    let toString = Units.toString Units.English Units.Long


    let toLangString loc = Units.toString loc Units.Short 



module CombiUnit =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib.BCL

    type CombiUnit = 
    | Combi of BigRational * Unit.Unit * (Operator * BigRational * Unit.Unit) list
    and Operator =
    | Per
    | Times

            
    let create v u = (v , u, []) |> Combi


    let get (Combi(v, u, ul)) = v, u, ul


    let operator op v u vu =
        let v', u', ul = vu |> get
        (v', u', ul @ [(op, v, u)]) 
        |> Combi


    let withUnit u v = create v u


    let per = operator Per
   
     
    let times = operator Times


    let getMultiplier vu = 
        let v, u, ul = vu |> get
        let mp v u = v * (u |> Unit.getMultiplier)

        ul 
        |> List.fold (fun acc (op, v, u) ->
            match op with
            | Per   -> acc / (mp v u)
            | Times -> acc * (mp v u)) (mp v u)

                        
    let toBase u v = v |> Unit.Multipliers.toBase (u |> getMultiplier)


    let toUnit u v = v |> Unit.Multipliers.toUnit (u |> getMultiplier)


    let eqGroup cu1 cu2 =
        let _, u1, ul1 = cu1 |> get
        let _, u2, ul2 = cu2 |> get
        u1 |> Unit.eqGroup u2 && 
        ul1 
        |> List.forall2 (fun (_, _, u1) (_, _, u2) -> u1 |> Unit.eqGroup u2) ul2


    let eval x =
        let _, u, ul = x |> get

        let sort xs =
            xs |> List.sortWith(fun x1 x2 -> 
                let op1, v1, _ = x1
                let op2, v2, _ = x2 
                match op1, op2 with
                | Times, Times -> if v1 > v2 then -1 else 0
                | Times, Per   -> -1
                | Per,  Times  -> +1
                | Per,  Per    -> 0)

        let eqs x1 x2 =
            let op1, _, u1 = x1
            let op2, _, u2 = x2
            let opeq = op1 = Per || op2 = Per
            let greq = u1 |> Unit.eqGroup u2
            opeq && greq 

        let rec simplify acc list = 
            let remCount xs = 
                xs 
                |> List.filter(fun x -> 
                    let (_, _, u) = x
                    u |> Unit.isCount |> not) 
                
            let rec remove i l =
                match i, l with
                | 0, x::xs -> xs
                | i, x::xs -> x::remove (i - 1) xs
                | i, [] -> failwith "index out of range"

            match list with
            | [] -> 
                let acc = acc |> remCount |> sort
                match acc with
                | [(Per, _, _)] -> (Times, 1N, Unit.count)::acc
                | _             -> acc
            | x::xs -> 
                match xs |> List.tryFindIndex (eqs x) with
                | Some i -> 
                    xs |> remove i |> simplify acc
                | None -> xs |> simplify (acc @ [x])
                    
        match simplify [] ((Times, 1N, u)::ul) with
        | [] -> create 1N Unit.count
        | x::xs -> 
            let _, v, u = x
            (v, u, xs) |> Combi


    let (|Mult|Div|Add|Subtr|) op =
        match op with
        | _ when 1N |> op <| 2N = 2N      -> Mult
        | _ when 1N |> op <| 2N = (1N/2N) -> Div
        | _ when 1N |> op <| 2N = 3N      -> Add
        | _ when 1N |> op <| 2N = -1N     -> Subtr
        | _ -> failwith "Not a valid operator"


    let calc op cu1 cu2 = 
        let toOp op = 
            match op with
            | Mult -> Times
            | Div  -> Per
            | _ -> failwith "Not a valid unit operator"

        let v1, u1, ul1 = cu1 |> get
        let v2, u2, ul2 = cu2 |> get

        match op with
        | Mult | Div ->
            (v1, u1, ul1 @ [op |> toOp, v2, u2] @ ul2) 
            |> Combi
            |> eval
        | Add | Subtr -> 
            if cu1 |> eqGroup cu2 then cu2
            else failwith "Cannot add units with different unit groups"


    let opToString = function
        | Per   -> Constants.divs
        | Times -> Constants.mults


    let opFromString s =
        match s with
        | _ when s = Constants.mults -> Times
        | _ when s = Constants.divs  -> Per
        | _ -> failwith "Not a valid operator string"


    let toString cu =
        let abbr = Unit.getAbbreviation
        let gr u = u |> Unit.getGroupName 
        let toStr u = (u |> abbr) + Constants.openBr + (u |> gr) + Constants.closBr

        let bigRatToString (v: BigRational) =
            if v = 1N then Constants.empts else v.ToString()

        let v, u, ul = cu |> get
        let acc = (v |> bigRatToString) + Constants.space + (u |> toStr) |> String.trim
        ul 
        |> List.fold (fun acc (o, v, u) -> 
                let v' = v |> bigRatToString
                let o' = o |> opToString
                let u' = u |> toStr
                acc +
                if v' = Constants.empts then o' + u' else o' + v' + Constants.space + u') acc


    let fromString s =
        let dels = "#"
        let getUnitAndGroup ug = 
            match ug |> String.replace Constants.closBr Constants.empts |> String.split Constants.openBr with
            | [u;g] -> u, g
            | _ -> sprintf "Could not parse unit from string: %s" ug |> failwith

        let ufs s =
            match s |> String.split Constants.space with
            | [ug] ->
                let u, g = ug |> getUnitAndGroup 
                match Unit.fromString u g with
                | Some (u') -> 1N, u'
                | None     -> failwith "Not a valid unit"
            | [v;ug] -> 
                let u, g = ug |> getUnitAndGroup 
                let v' = v |> BigRational.Parse
                match Unit.fromString u g with
                | Some (u') -> v', u'
                | None     -> failwith "Not a valid unit"
            | _ -> failwith "Cannot parse string"

        let rec parse ul usl =
            match usl with
            | [us] -> 
                let v, u = us |> ufs
                (v, u, ul) |> Combi
            | us::os::rest -> 
                let v, u = us |> ufs
                let o = os |> opFromString
                rest |> parse ([ (o, v, u)] @ ul)
            | _ -> failwith "Cannot parse string list"

        s 
        |> String.replace Constants.mults (dels + Constants.mults + dels) 
        |> String.replace Constants.divs  (dels + Constants.divs + dels)
        |> String.split dels
        |> List.rev
        |> parse []
        

    let toLangString lang prec cu =
        let toStr u = Unit.toLangString lang u

        let bigRatToString (v: BigRational) =
            if v = 1N then Constants.empts else v |> BigRational.toFloat |> Double.fixPrecision prec |> string

        let v, u, ul = cu |> get
        let acc = (v |> bigRatToString) + Constants.space + (u |> toStr) |> String.trim
        ul 
        |> List.fold (fun acc (o, v, u) -> 
                let v' = v |> bigRatToString
                let o' = o |> opToString
                let u' = u |> toStr
                acc +
                if v' = Constants.empts then o' + u' else o' + v' + Constants.space + u') acc


    type CombiUnit with
        
        static member (*) (cu1, cu2) = calc (*) cu1 cu2

        static member (/) (cu1, cu2) = calc (/) cu1 cu2

        static member (+) (cu1, cu2) = calc (+) cu1 cu2

        static member (-) (cu1, cu2) = calc (-) cu1 cu2


    module Units = 

        open Unit 

        let countTimes n = Count Times |> create n


        let massKiloGram n = Mass KiloGram |> create n


        let massGram n = Mass Gram |> create n


        let massMilliGram n = Mass MilliGram |> create n


        let massMicroGram n = Mass MicroGram |> create n


        let massNanoGram n = Mass NanoGram |> create n


        let volumeLiter n = Volume Liter |> create n


        let volumeDeciLiter n = Volume DeciLiter |> create n

         
        let volumeMilliLiter n = Volume MilliLiter |> create n

         
        let volumeMicroLiter n = Volume MicroLiter |> create n


        let timeYear n = Time Year |> create n


        let timeMonth n = Time Month |> create n


        let timeWeek n = Time Week |> create n


        let timeDay n = Time Day |> create n


        let timeHour n = Time Hour |> create n


        let timeMinute n = Time Minute |> create n

         
        let timeSecond n = Time Second |> create n


        let molarMol n = Molar Mol |> create n


        let molarMilliMol n = Molar MilliMol |> create n

                         
        let weightKg n = Weight WeightKilogram |> create n


        let weightGram n = Weight WeightGram |> create n


        let bsaM2 n = BSA M2 |> create n


module ValueUnit =

    open MathNet.Numerics

    open Informedica.GenUtils.Lib.BCL

    module US = Unit
    module CS = Constants
    module UN = Unit
    module CU = CombiUnit

    type ValueUnit = ValueUnit of BigRational * CU.CombiUnit


    let create v u = (v, u) |> ValueUnit


    let get (ValueUnit(v, u)) = v, u


    let calc op vu1 vu2 =
        let v1, u1 = vu1 |> get
        let v2, u2 = vu2 |> get
        let u = CU.calc op u1 u2
        let v = v1 |> CU.toBase u1 |> op <| (v2 |> CU.toBase u2) |> CU.toUnit u
        create v u


    let cmp cp vu1 vu2 =
        let v1, u1 = vu1 |> get
        let v2, u2 = vu2 |> get
        v1 |> CU.toBase u1 |> cp <| (v2 |> CU.toBase u2)


    let canConvert cu vu =
        let v, cu1 = vu |> get
        let _, u1, ul1 = cu1 |> CU.get
        let _, u2, ul2 = cu  |> CU.get

        let eq u1 u2 = u1 |> UN.getGroupName = (u2 |> UN.getGroupName)

        let canConvUl ul1 ul2 =
            ul1 |> List.forall2 (fun (o1, _, u1) (o2, _, u2) ->
                o1 = o2 && u1 |> eq u2
            ) ul2

        u1 |> eq u2 && canConvUl ul1 ul2     


    let convertTo cu vu =
        let v, cu1 = vu |> get
        let _, u1, ul1 = cu1 |> CU.get
        let _, u2, ul2 = cu  |> CU.get

        let v' = v |> CU.toBase cu1 |> CU.toUnit cu
        (v', cu) |> ValueUnit


    let toString vu =
        let v, u = vu |> get
        v.ToString() + " " + (u |> CU.toString)


    let toLangString lang prec vu =
        let v, u = vu |> get
        (v |> BigRational.toFloat 
           |> Double.fixPrecision prec
           |> string) + " " + (u |> CU.toLangString lang 1)


    let toFloatString prec vu =
        let v, u = vu |> get
        (v |> BigRational.toFloat |> Double.fixPrecision prec |> string) + " " + (u |> CU.toString)


    let fromString s =
        match s |> String.split CS.space with
        | vs::_ ->
            let v = vs |> BigRational.Parse
            let rest = s |> String.subString (vs |> String.length) ((s |> String.length) - (vs |> String.length))
            let cu = 
                rest
                |> String.trim
                |> CU.fromString
            (v, cu) |> ValueUnit
        | _ -> failwith "Cannot parse string"


    type ValueUnit with

        static member (*) (vu1, vu2) = calc (*) vu1 vu2

        static member (/) (vu1, vu2) = calc (/) vu1 vu2

        static member (+) (vu1, vu2) = calc (+) vu1 vu2

        static member (-) (vu1, vu2) = calc (-) vu1 vu2

        static member op_Equal (vu1, vu2) = cmp (=) vu1 vu2

        static member op_GreaterThan (vu1, vu2) = cmp (>) vu1 vu2

        static member op_SmallerThan (vu1, vu2) = cmp (<) vu1 vu2

        static member op_GreaterThanOrEqual (vu1, vu2) = cmp (>=) vu1 vu2

        static member op_SmallerThanOrEqual (vu1, vu2) = cmp (<=) vu1 vu2


open MathNet.Numerics

CombiUnit.Units.massMicroGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeMinute 2N)
|> CombiUnit.toString
|> CombiUnit.fromString


let v1 =
    ValueUnit.create 5N (CombiUnit.Units.massMicroGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeMinute 1N))

let v2 =
    ValueUnit.create 100N (CombiUnit.Units.massMilliGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeHour 1N))

v2 / v1

v2
|> ValueUnit.convertTo (CombiUnit.Units.massMicroGram 1N / (CombiUnit.Units.weightKg 1N) / (CombiUnit.Units.timeMinute 1N))
|> (fun vu -> vu / v1)

