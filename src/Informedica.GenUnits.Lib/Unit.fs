namespace Informedica.GenUnits.Lib


module Unit =

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL
    open MathNet.Numerics

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

        let inline toBase m v  = v * m
        let inline toUnit m v  = v / m


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
        | Count _ -> Multipliers.one
        | Mass g  -> 
            match g with
            | KiloGram -> Multipliers.kilo
            | Gram -> Multipliers.one
            | MilliGram -> Multipliers.milli
            | MicroGram -> Multipliers.micro
            | NanoGram -> Multipliers.nano
        | Volume g  ->
            match g with
            | Liter -> Multipliers.one
            | DeciLiter -> Multipliers.deci
            | MilliLiter -> Multipliers.milli
            | MicroLiter -> Multipliers.micro
        | Time g  ->
            match g with
            | Year -> Multipliers.year
            | Month -> Multipliers.month
            | Week -> Multipliers.week
            | Day -> Multipliers.day
            | Hour -> Multipliers.hour
            | Minute -> Multipliers.minute
            | Second -> Multipliers.second
        | Molar g  ->
            match g with
            | Mol -> Multipliers.one
            | MilliMol -> Multipliers.milli
        | InternationalUnit g  ->
            match g with
            | MiljIU -> Multipliers.kilo * Multipliers.kilo
            | IU -> Multipliers.one
        | Weight g  -> 
            match g with
            | WeightKilogram -> Multipliers.kilo
            | WeightGram -> Multipliers.one
        | Height g  -> 
            match g with
            | Meter -> Multipliers.one
            | CentiMeter -> Multipliers.centi
        | BSA g  -> 
            match g with
            | M2 -> Multipliers.one


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


        let getUnit ud = (ud |> get).Unit


        let create un gr ab nm sy = 
            {
                Unit = un
                Group = gr
                Abbreviation = ab
                Name = nm
                Synonyms = sy
            }


        let createGeneral s = 
            let un = s |> General
            let ab = { Eng = s; Dut = s }
            let nm = { Eng = s; Dut = s }

            create un "General" ab nm []


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

